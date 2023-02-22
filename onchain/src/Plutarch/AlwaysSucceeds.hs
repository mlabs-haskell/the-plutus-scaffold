
module Plutarch.AlwaysSucceeds where

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts
-- import Plutarch (compile)

alwaysSucceeds :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \datm redm ctx -> pconstant ()