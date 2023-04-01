{-# LANGUAGE UndecidableInstances #-}

module Exporter.Types where

import Data.Kind (Type)

import Control.Monad.State (StateT)
import Data.Text (Text)
import Data.Map (Map)
import Ply (
  ScriptVersion(..),
  ScriptRole(..), TypedScriptEnvelope,
 )
import PlutusTx.Code ( CompiledCode )

type App = StateT (Map Text ScriptEnvelope) (Either Text)

data Some :: (k -> Type) -> Type where
  MkSome ::  f a -> Some f

data ScriptEnvelope
 = PlutusScriptEnvelope PlutusEnvelope
 | PlutarchScriptEnvelope TypedScriptEnvelope

data PlutusEnvelope = PlutusEnvelope {
  peRole        :: !ScriptRole,
  peVersion     :: !ScriptVersion,
  peDescription :: !Text,
  peScript      :: !(Some CompiledCode)
}

mkPlutusEnvelope :: ScriptRole -> ScriptVersion -> Text -> CompiledCode a ->  PlutusEnvelope
mkPlutusEnvelope  role version descr compiled
 = PlutusEnvelope role version descr (MkSome compiled)
