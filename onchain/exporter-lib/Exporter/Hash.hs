{-# LANGUAGE RecordWildCards #-}
module Exporter.Hash (HasScriptHash(scriptHashOf),showHash) where

import Plutarch.Api.V1 ( scriptHash )
import Plutarch.Script ( Script(Script) )
import Ply (
  ScriptVersion(..),
  TypedScriptEnvelope (
    TypedScriptEnvelope,
    tsDescription,
    tsParamTypes,
    tsRole,
    tsScript,
    tsVersion
  ),
 )
import qualified Data.Text as T
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Short (fromShort)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import PlutusLedgerApi.V1 (ScriptHash(..))
import PlutusTx.Prelude (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusLedgerApi.Common ( serialiseCompiledCode )
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (
  Blake2b_224 (Blake2b_224),
  HashAlgorithm,
 )
import Exporter.Types
    ( PlutusEnvelope(..), Some(MkSome), ScriptEnvelope(..) )

class HasScriptHash a where
  scriptHashOf :: a -> ScriptHash

instance HasScriptHash TypedScriptEnvelope where
  scriptHashOf TypedScriptEnvelope{..} = scriptHash (Script tsScript)

instance HasScriptHash PlutusEnvelope where
  scriptHashOf PlutusEnvelope{..} = case peScript of
    MkSome script -> mkScriptHash . fromShort . serialiseCompiledCode  $ script
   where
     mkScriptHash ::  ByteString -> ScriptHash
     mkScriptHash bs
       = ScriptHash
           . hashBlake2b_224
           $ prefix <> bs

     -- NOTE: Based on https://github.com/input-output-hk/cardano-ledger/blob/c721b2a002367548312cdda15e513f3df197cb56/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Scripts.hs#L193-L202
     --       I *think* these prefixes are correct, though I'm not certain and this **MAY BE WRONG**
     prefix :: ByteString
     prefix = case peVersion of
       ScriptV1 -> "\x01"
       ScriptV2 -> "\x02"

     -- Plutarch doesn't export these but we need them
     _plutusHashWith :: HashAlgorithm alg => alg -> ByteString -> BuiltinByteString
     _plutusHashWith alg = toBuiltin . convert @_ @ByteString . hashWith alg

     hashBlake2b_224 :: ByteString -> BuiltinByteString
     hashBlake2b_224 = _plutusHashWith Blake2b_224

instance HasScriptHash ScriptEnvelope where
  scriptHashOf = \case
    PlutarchScriptEnvelope pe -> scriptHashOf pe
    PlutusScriptEnvelope pe -> scriptHashOf pe

showHash :: ScriptHash -> String
showHash =
  T.unpack
    . T.decodeUtf8
    . Base16.encode
    . fromBuiltin
    . getScriptHash
