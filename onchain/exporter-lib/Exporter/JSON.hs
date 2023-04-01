{-# LANGUAGE RecordWildCards #-}
module Exporter.JSON () where

import Ply (
  ScriptVersion(..), TypedScriptEnvelope(..),
 )
import Data.Text (Text)
import Data.Aeson
    ( object, Value(String, Object), KeyValue((.=)), ToJSON(toJSON) )
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import qualified Cardano.Binary as CBOR
import PlutusLedgerApi.Common ( serialiseCompiledCode )
import qualified Data.Aeson.KeyMap as KM
import Exporter.Types
    ( PlutusEnvelope(..), Some(MkSome), ScriptEnvelope(..) )

versionStr :: ScriptVersion -> Text
versionStr = \case
       ScriptV1 -> "PlutusScriptV1"
       ScriptV2 -> "PlutusScriptV2"


instance ToJSON PlutusEnvelope where
  toJSON PlutusEnvelope{..} =
    object ["type"        .= (versionStr peVersion)
           ,"description" .= peDescription
           ,"cborHex"     .= T.decodeUtf8 (Base16.encode serialized)]
   where
     serialized :: ByteString
     serialized = case peScript of
       MkSome script -> CBOR.serialize' . serialiseCompiledCode $ script

{-

If we add a "type" field to the JSON serialization of the TypedScriptEnvelope, it should be readable
by anything that can read a cardano-api style script envelope (while still being readable by
tools that expect Ply-style envelopes)

-}
instance ToJSON ScriptEnvelope where
  toJSON = \case
    PlutarchScriptEnvelope tenv@TypedScriptEnvelope{..} -> case toJSON tenv of
      Object keymap -> Object $ KM.insert "type" (String $ versionStr tsVersion) keymap
      _             -> error "Error: TypedScriptEnvelope serialized to something other than a JSON Object!"
    PlutusScriptEnvelope pe -> toJSON pe
