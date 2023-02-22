{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cardano.Api (PlutusScriptV2, serialiseToJSON, serialiseToTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Control.Applicative ((<|>))
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Debug.Trace (trace)
import Ledger (scriptHash)
import Plutus.V2.Ledger.Api (Script)
import System.Environment (getArgs, lookupEnv)
import Prelude

import AlwaysSucceeds qualified

data ScriptsFFI = ScriptsFFI
    { js :: String
    , purs :: String
    }

-- | Generates target JS and Purs files
mkScriptsFFI :: [(String, Script)] -> ScriptsFFI
mkScriptsFFI scripts = ScriptsFFI{js, purs}
  where
    purs =
        unlines $
            ["module MLabsPlutusTemplate.ScriptsFFI (" <> intercalate "," (fst <$> scripts) <> ") where\n"]
                ++ ((\(rawName, _) -> "foreign import " ++ rawName ++ " :: String") <$> scripts)
    js = unlines $ scriptToDeclaration <$> scripts
    scriptToDeclaration (rawName, script) =
        trace ("generating " <> rawName <> " with hash = " <> show (scriptHash script)) $
            "exports." <> rawName <> " = " <> show (scriptToString script) <> ";"
    scriptToString =
        Text.unpack . Text.decodeUtf8 . serialiseToJSON
            . serialiseToTextEnvelope Nothing
            . PlutusScriptSerialised @PlutusScriptV2
            . toShort
            . toStrict
            . serialise

main :: IO ()
main = do
    out <- do
        argOut <- listToMaybe <$> getArgs
        envOut <- lookupEnv "out"
        pure $ maybe "." id $ argOut <|> envOut
    let ScriptsFFI{js,purs} =
            mkScriptsFFI
                [ ("AlwaysSucceeds", AlwaysSucceeds.script)
                -- , ("adminNftMintingPolicy", NftMintingPolicy.script)
                ]
    writeFile (out <> "/ScriptsFFI.js") js
    writeFile (out <> "/ScriptsFFI.purs") purs
