module MlabsPlutusTemplate.Api
  (
    square
  , module Export
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)
import MLabsPlutusTemplate.Scripts (always_succeeds) as Export
-- import Contract.JsSdk
--   ( runContractJS
--   )

square :: Fn1 Int Int
square = mkFn1 $ \n -> n*n
