module MlabsPlutusTemplate.Api
  (
    square
  -- , module Export
  )
  where

import Prelude
import Data.Function.Uncurried (Fn1, mkFn1)
-- import 
-- import MLabsPlutusTemplate.Scripts (always_succeeds) as Export
-- import Contract.JsSdk
--   ( runContractJS
--   )
-- import Data.Int (pow)

square :: Fn1 Int Int
square = mkFn1 $ \n -> n * n
