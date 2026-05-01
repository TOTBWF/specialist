{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# LANGUAGE BlockArguments #-}

module GHC.Specialist.Plugin.Instrumentation where

import GHC.Specialist.CostCentre
import GHC.Specialist.Plugin.Types

import GHC.IO.Handle.Text
import GHC.IO.StdHandles qualified as Handles

import Control.Concurrent
import Control.Monad

import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Base64 qualified as LBS
import Data.Binary qualified as Bin
import Data.List

import Debug.Trace.ByteString qualified as Trace

import GHC.Exts
import GHC.Exts.Heap
import GHC.InfoProv
import GHC.IORef
import GHC.IO
import System.Random
import Unsafe.Coerce


-- | Put a dictionary in a box
{-# NOINLINE dictToBox #-}
dictToBox :: forall c. Dict c -> Box
dictToBox = unsafeCoerce

-- | Put a dictionary as a constraint in a box
mkBox :: forall a. a => Box
mkBox = dictToBox (Dict @a)

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing:xs) = catMaybes' xs
catMaybes' (Just x:xs) =
  let !rest = catMaybes' xs
  in x:rest

-- | Traverse free variables of a dictionary to determine the superclasses. For
-- some reason, the references to superclass dictionaries (sometimes?) go
-- through the class functions, so we need to follow references through the
-- functions and thunks without adding the functions or thunks themselves as
-- superclasses.
--
-- Additionally, we would like to avoid adding the same superclass twice if it
-- is referenced by two class functions. I.e., we only want to add superclasses
-- encountered on distinct paths through other superclasses, not distinct paths
-- through class functions. Therefore we accumulate the superclasses we have
-- encountered as direct references from a closure in a set.
getDictInfo :: (Box, String) -> IO DictInfo
getDictInfo (box@(Box dict), prettyType) = do
    dc <-
      getClosureData dict >>=
        \case
          ConstrClosure _ ptrs _ _ _ dcon_nm | 'C':':':cls_nm <- dcon_nm -> do
            wf <- whereFrom dict
            frees <- go (classNameFilt cls_nm) [] ptrs
              -- go (classNameFilt cls_nm) ptrs _
              -- evaluate =<< catMaybes' <$> mapM (go (classNameFilt cls_nm)) ptrs
            return $ DictClosure wf frees
          FunClosure _ ptrs _ -> do
            -- Assume this is a single method dictionary, which is actually just
            -- the function
            wf <- whereFrom dict
            frees <- go (const True) [] ptrs
            return $ DictClosure wf frees
          ThunkClosure _ ptrs _ -> do
            -- TODO: For some reason, some dictionaries are showing up as
            -- thunks. If we force them, I think some will show up as PAPs?
            wf <- whereFrom dict
            frees <- go (const True) [] ptrs
            return $ DictClosure wf frees
          _ ->
            go (const True) [] [box] >>=
              \case
                [dc] ->
                  return dc
                _ -> do
                  wf <- whereFrom dict
                  return (DictClosureRaw wf)

    return $ DictInfo prettyType dc
  where
    go :: (InfoProv -> Bool) -> [DictClosure] -> [Box] -> IO [DictClosure]
    go _ipeFilt acc [] = pure acc
    go ipeFilt acc (Box d:rest) =
      getClosureData d >>= \case
          ConstrClosure _ ptrs _ _ _ dcon_nm | 'C':':':cls_nm <- dcon_nm -> do
            wf <- whereFrom d
            frees <- go (classNameFilt cls_nm) [] ptrs
            go ipeFilt (DictClosure wf frees:acc) rest
          FunClosure _ ptrs _ -> do
            -- Assume this is a single method dictionary, which is actually just
            -- the function
            --
            -- If the filter is "False", then we assume this is a function
            -- referenced by a dictionary, so we don't want to keep it
            --
            -- TODO: Differentiating single-method type class dictionaries and
            -- regular functions is difficult to do precisely, and this doesn't
            -- properly do it. We still get a lot of false positives (regular
            -- functions considered dictionaries). I think it is better to err
            -- on this side for now, but we should figure out how to be a bit
            -- more precise with it. Ideas: Are single method type class
            -- dictionaries ever given "sat_" names? If not, we could filter on
            -- those too.
            wf <- whereFrom d
            if maybe True ipeFilt wf then do
              frees <- go (const True) [] ptrs
              go ipeFilt (DictClosure wf frees:acc) rest
            else
              go ipeFilt acc rest
          IndClosure _ ptr -> do
            -- Go straight through indirections
            --
            -- I believe this can happen if, e.g., a dictionary is given a cost
            -- center with -fprof-late.
            go ipeFilt acc (ptr:rest)
          BlackholeClosure _ ind -> do
            -- Blackholes can be updated to point to indirections, if things are
            -- timed just right. See Note [BLACKHOLE pointing to IND] in GHC.
            ind_cd <- getClosureData ind
            case ind_cd of
              IndClosure _ ptr ->
                go ipeFilt acc (ptr:rest)
              _ ->
                -- TODO: When the plugin is enabled on the Cabal-syntax library,
                -- and we run the cabal hackage-test parsec test, we see many
                -- dictionaries come through as BlackholeClosures. It's unclear
                -- what exactly is resulting in this, but as a first-try
                -- workaround I added this retry loop (instead of `return
                -- Nothing`) and it fixed it. Definitely worth keeping in mind,
                -- in case this ends up biting us later.
                let !d' = d in go ipeFilt acc (Box d':rest)
          _c -> do
            go ipeFilt acc rest

    -- If the class name of this closure is found in ptr closures, assume it is
    -- actually a class function, not a superclass
    --
    -- className will look like "Eq_Main_0_con_info" and we want to filter out
    -- references to things like "$fEqX_$c==_info", so we look for the "Eq".
    -- We also attempt to filter references to other non-dictionaries by
    classNameFilt :: String -> InfoProv -> Bool
    classNameFilt className InfoProv{..} =
        not $ takeWhile (/= '_') className `isInfixOf` ipName

-- | Trace a 'SpecialistNote' to the eventlog.
--
-- This function should be preferred over 'traceEventIO', which
-- can only accept a 'String'. This makes it tempting to 'show'
-- a 'SpecialistNote', which very quickly will bump into the 64k
-- limitation that is EVENT_PAYLOAD_SIZE_MAX.
traceSpecialistNote :: SpecialistNote -> IO ()
traceSpecialistNote note = do
  enc <- evaluate $ LBS.toStrict $ extractBase64 $ LBS.encodeBase64' $ Bin.encode note
  let len = BS.length enc
  if len < 2^(16 :: Int) then
    Trace.traceEventIO enc
  else
    hPutStrLn Handles.stderr $ "WARNING: serialized specialist event exceeded EVENT_PAYLOAD_SIZE_MAX: " <> show len <> " bytes"

{-# NOINLINE specialistWrapper' #-}
specialistWrapper' :: forall a r (b :: TYPE r).
     Double
  -> IORef Bool
  -> Addr#
  -> Addr#
  -> Addr#
  -> (a -> b)
  -> [(Box, String)]
  -> ()
specialistWrapper' sampleProb hasSampledRef fIdAddr lAddr ssAddr f boxedDicts =
    unsafePerformIO $ do
      coin <- (< sampleProb) <$> randomRIO @Double (0.0, 1.0)
      sampled <- atomicModifyIORef' hasSampledRef (True,)
      when (coin || not sampled) $
        traceSpecialistNote =<<
          SpecialistNote (unpackCString# fIdAddr)
            <$> currentCallStack
            <*> (reverse . map fromIntegral <$> currentCallStackIds)
            <*> mapM getDictInfo boxedDicts
            <*> whereFrom f
            <*> pure (unpackCString# lAddr)
            <*> pure (unpackCString# ssAddr)
            <*> fmap (fromIntegral . fst) (myThreadId >>= threadCapability)

specialistWrapper :: forall a r (b :: TYPE r).
     Double
  -- ^ Sample probability
  -> IORef Bool
  -- ^ 'IORef' holding a 'Bool' indicating whether a sample has been emitted for
  -- this overloaded call yet.
  -> Addr#
  -- ^ Unique identifier for this overloaded call site
  -> Addr#
  -- ^ Label of the last source tick we encountered
  -> Addr#
  -- ^ Source span of the last source tick we encountered
  -> (a => b)
  -- ^ The overloaded function
  -> [(Box, String)]
  -- ^ 'Box'es holding the dictionaries used in the overloaded call, paired with
  -- their pretty-printed types
  -> ()
specialistWrapper = unsafeCoerce specialistWrapper'

-- | Just here to call @exprType@ on
boxTypeDUMMY :: Box
boxTypeDUMMY = error "I'm just here to be a Type, do not evaluate"

-- | Just here to call @exprType@ on
iorefBoolTypeDUMMY :: IORef Bool
iorefBoolTypeDUMMY = error "I'm just here to be a Type, do not evaluate"

boolTypeDUMMY :: Bool
boolTypeDUMMY = error "I'm just here to be a Type, do not evaluate"
