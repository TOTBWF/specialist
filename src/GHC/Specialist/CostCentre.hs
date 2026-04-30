{-# LANGUAGE DerivingStrategies #-}
-- | Cost-centre patches taken from <https://gitlab.haskell.org/ghc/ghc/-/commit/b30f6349a01028e3fd13b6dd1d706b1ec612ffe5>.
module GHC.Specialist.CostCentre
  ( CostCentreId
  , ccId
  , ccsToIds
  , currentCallStackIds
  ) where

import GHC.Internal.Foreign.C.String.Encoding as GHC
import GHC.Internal.Foreign.Storable

import GHC.Internal.IO.Encoding

import GHC.Internal.Ptr
import GHC.Internal.Stack
  ( CostCentre
  , ccLabel, ccModule
  , CostCentreStack
  , getCurrentCCS, ccsCC, ccsParent
  )
import GHC.Internal.Word (Word32)

-- | Cost centre identifier
--
-- @since 0.2.0
newtype CostCentreId = CostCentreId Word32
  deriving (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum, Integral, Num, Real)

-- | Get the 'CostCentreId' of a 'CostCentre'.
--
-- @since 0.2.0
ccId :: Ptr CostCentre -> IO CostCentreId
ccId p = fmap CostCentreId $ peekByteOff p 0

-- | Returns a @[CostCentreId]@ representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintained by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-late@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- @since 4.20.0.0
currentCallStackIds :: IO [CostCentreId]
currentCallStackIds = ccsToIds =<< getCurrentCCS ()

-- | Format a 'CostCentreStack' as a list of cost centre IDs.
--
-- @since 4.20.0.0
ccsToIds :: Ptr CostCentreStack -> IO [CostCentreId]
ccsToIds ccs0 = go ccs0 []
  where
    go ccs acc
     | ccs == nullPtr = return acc
     | otherwise = do
        cc <- ccsCC ccs
        cc_id <- ccId cc
        lbl <- GHC.peekCString utf8 =<< ccLabel cc
        mdl <- GHC.peekCString utf8 =<< ccModule cc
        parent <- ccsParent ccs
        if (mdl == "MAIN" && lbl == "MAIN")
           then return acc
           else go parent (cc_id : acc)
