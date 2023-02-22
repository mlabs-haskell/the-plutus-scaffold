{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

-- TODO: V1/V2 split

module Plutus.AlwaysSucceeds where

import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.Prelude (Bool (True), ($))
import Ledger.Typed.Scripts (ValidatorTypes(..), Validator)
import qualified PlutusTx
import Ledger (Address, fromCompiledCode, Validator (Validator), validatorHash, scriptHashAddress)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (TypedValidator)
import Plutus.V2.Ledger.Api (unValidatorScript, Script)

script :: Script
script = unValidatorScript compiled

{-# INLINEABLE mkAlwaysSucceeds #-}
mkAlwaysSucceeds :: () -> () -> ScriptContext -> Bool
mkAlwaysSucceeds _ _ _ = True

data AlwaysSucceeds

instance ValidatorTypes AlwaysSucceeds where
  type DatumType AlwaysSucceeds = ()
  type RedeemerType AlwaysSucceeds = ()

typedValidator :: TypedValidator AlwaysSucceeds
typedValidator =
  Scripts.mkTypedValidator @AlwaysSucceeds
    $$(PlutusTx.compile [||mkAlwaysSucceeds||])
    $$(PlutusTx.compile [||wrap||])
  where
    {-# INLINEABLE wrap #-}
    wrap = Scripts.mkUntypedValidator @() @()

compiled :: Validator
compiled = Validator $ fromCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceeds||])

-- validator :: Versioned Validator
-- validator = Versioned compiled PlutusV1

validatorAddr :: Address
validatorAddr = scriptHashAddress (validatorHash compiled)