{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module GHC.Specialist.Plugin.Orphans where

import Data.Binary

-- import Data.Text
-- import GHC.Types.DumpSpecInfo
import GHC.Internal.ClosureTypes
import GHC.InfoProv
import GHC.Generics

-- deriving instance Eq (DumpSpecInfo Text Text Text)
-- deriving instance Ord (DumpSpecInfo Text Text Text)
-- deriving instance Show (DumpSpecInfo Text Text Text)
-- deriving instance Read (DumpSpecInfo Text Text Text)

deriving instance Ord InfoProv

deriving instance Generic InfoProv
deriving instance Binary InfoProv

deriving instance Binary ClosureType
