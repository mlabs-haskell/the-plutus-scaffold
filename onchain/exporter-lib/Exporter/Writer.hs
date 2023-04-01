{-# LANGUAGE RecordWildCards #-}
module Exporter.Writer (
  runExporter,
  writeScriptEnvelope,
  savePlutarchScript,
  savePlutarchScript_,
  savePlutusV1MintingPolicy,
  savePlutusV2MintingPolicy,
  savePlutusV1Validator,
  savePlutusV2Validator,
  savePlutusV1MintingPolicy_,
  savePlutusV2MintingPolicy_,
  savePlutusV1Validator_,
  savePlutusV2Validator_,
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.State (execStateT, modify')
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Text (Text)
import GHC.IO (throwIO)
import Data.Aeson.Encode.Pretty (encodePretty)

import Plutarch
import Ply (
  ScriptVersion(..),
  ScriptRole(..),
 )
import Ply.Plutarch (mkEnvelope)
import Ply.Plutarch.TypedWriter (TypedWriter)
import PlutusLedgerApi.V1 (ScriptHash(..))
import PlutusTx.Code ( CompiledCode )
import Exporter.Types
    ( PlutusEnvelope(PlutusEnvelope),
      Some(MkSome),
      App,
      ScriptEnvelope(..) )
import Exporter.Hash ( HasScriptHash(scriptHashOf), showHash )
import Exporter.JSON ()

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text as T

writeScriptEnvelope :: FilePath -> ScriptEnvelope -> IO ()
writeScriptEnvelope path = LBS.writeFile path . encodePretty

savePlutarchScript_ :: TypedWriter pt => Text -> ClosedTerm pt -> App ()
savePlutarchScript_ nm pt= void $ savePlutarchScript nm pt

savePlutarchScript :: TypedWriter pt => Text -> ClosedTerm pt -> App ScriptHash
savePlutarchScript nm pt = case PlutarchScriptEnvelope <$> mkEnvelope (Config {tracingMode = DoTracing}) nm pt of
  Left err -> throwError err
  Right env -> do
    modify' $ M.insert nm env
    pure $ scriptHashOf  env

{-
    NOTE: The plutus variants do not check that their arguments really are the type/version users assert
          them to be.
-}
savePlutusV1MintingPolicy :: Text -> CompiledCode a -> App ScriptHash
savePlutusV1MintingPolicy nm code = do
  modify' $ M.insert nm env
  pure $ scriptHashOf env
 where
   env = PlutusScriptEnvelope $ PlutusEnvelope MintingPolicyRole ScriptV1 nm (MkSome code)

savePlutusV1MintingPolicy_ :: Text -> CompiledCode a -> App ()
savePlutusV1MintingPolicy_ nm code = void $ savePlutusV1MintingPolicy nm code

savePlutusV2MintingPolicy :: Text -> CompiledCode a -> App ScriptHash
savePlutusV2MintingPolicy nm code = do
  modify' $ M.insert nm env
  pure $ scriptHashOf env
 where
   env = PlutusScriptEnvelope $ PlutusEnvelope MintingPolicyRole ScriptV2 nm (MkSome code)

savePlutusV2MintingPolicy_ :: Text -> CompiledCode a -> App ()
savePlutusV2MintingPolicy_ nm code = void $ savePlutusV2MintingPolicy nm code

savePlutusV1Validator :: Text -> CompiledCode a -> App ScriptHash
savePlutusV1Validator nm code = do
  modify' $ M.insert nm env
  pure $ scriptHashOf env
 where
   env = PlutusScriptEnvelope $ PlutusEnvelope ValidatorRole ScriptV1 nm (MkSome code)

savePlutusV1Validator_ :: Text -> CompiledCode a -> App ()
savePlutusV1Validator_ nm code = void $ savePlutusV1Validator nm code

savePlutusV2Validator :: Text -> CompiledCode a -> App ScriptHash
savePlutusV2Validator nm code = do
  modify' $ M.insert nm env
  pure $ scriptHashOf env
 where
   env = PlutusScriptEnvelope $ PlutusEnvelope ValidatorRole ScriptV2 nm (MkSome code)

savePlutusV2Validator_ :: Text -> CompiledCode a -> App ()
savePlutusV2Validator_ nm code = void $ savePlutusV2Validator nm code



runExporter :: FilePath -> App () -> IO ()
runExporter dir exporter = do
  createDirectoryIfMissing True dir
  scriptsMap <- either (throwIO . userError . T.unpack) pure $ flip execStateT M.empty exporter
  putStrLn $ "Writing " <> show (length scriptsMap) <> " scripts"
  let scriptsMapWithHashes = M.map (\env -> (env, scriptHashOf env)) scriptsMap
  -- save plutus script files
  traverse_ saveScript $ M.toList scriptsMapWithHashes
  putStrLn $ "Writing Index.json"
  -- save json "name : hash" mapping
  writeIndex $ M.map (showHash . snd) scriptsMapWithHashes
  putStrLn "Done"
  where
    saveScript :: (Text, (ScriptEnvelope, ScriptHash)) -> IO ()
    saveScript (nm, (env, hash)) = do
      let hashStr = showHash hash
      putStrLn $ "Writing " <> T.unpack nm <> " to " <> dir </> hashStr <> ".plutus"
      writeScriptEnvelope
        (dir </> hashStr <> ".plutus")
        env

    writeIndex :: Map Text String -> IO ()
    writeIndex = LBS.writeFile filepath . encodePretty
      where
        filepath = dir </> "Index.json"
