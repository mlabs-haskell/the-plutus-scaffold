{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import System.FilePath ((</>))

import Control.Monad.State (StateT, execStateT, modify')
import Plutarch
import Plutarch.Api.V1
import Plutarch.Script
import Ply (
  TypedScriptEnvelope (
    TypedScriptEnvelope,
    tsDescription,
    tsParamTypes,
    tsRole,
    tsScript,
    tsVersion
  ),
 )
import Ply.Plutarch (mkEnvelope)
import Ply.Plutarch.TypedWriter (TypedWriter)

import Data.Text (Text)
import qualified Data.Text as T

import Ply.Core.Serialize (writeEnvelope)
import System.Directory (listDirectory, removeFile)

import Control.Monad (foldM)

-- import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import GHC.IO (throwIO)
import Plutarch.Lift (pconstant)
import PlutusLedgerApi.V1 (ScriptHash (getScriptHash))
import PlutusTx.Prelude (fromBuiltin)

-- import Ply.Core.Deserialize (readEnvelope)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import Plutarch.ExampleContracts (alwaysSucceeds, nftMp)

main :: IO ()
main = runExporter $ do
  hash <- save "always succeeds" alwaysSucceeds
  save_ "nft (hash applied)" (nftMp # pconstant hash)
  save_ "nft (no hash applied)" nftMp

type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

type App = StateT (Map Text TypedScriptEnvelope) (Either Text)

hashScript :: UPLCProgram -> ScriptHash
hashScript = scriptHash . Script

hashTypedEnvelope :: TypedScriptEnvelope -> ScriptHash
hashTypedEnvelope TypedScriptEnvelope {..} = hashScript tsScript

save_ :: TypedWriter pt => Text -> ClosedTerm pt -> App ()
save_ nm pt = case mkEnvelope (Config {tracingMode = DoTracing}) nm pt of
  Left err -> throwError err
  Right env -> modify' $ M.insert nm env

save :: TypedWriter pt => Text -> ClosedTerm pt -> App ScriptHash
save nm pt = case mkEnvelope (Config {tracingMode = DoTracing}) nm pt of
  Left err -> throwError err
  Right env -> do
    modify' $ M.insert nm env
    pure $ hashTypedEnvelope env

showHash :: ScriptHash -> String
showHash =
  T.unpack
    . T.decodeUtf8
    . Base16.encode
    . fromBuiltin
    . getScriptHash

runExporter :: App () -> IO ()
runExporter exporter = do
  deleteOldScripts -- I think we want to do this, so users don't have to do it manually
  toWrite <- either (throwIO . userError . T.unpack) pure $ flip execStateT M.empty exporter
  putStrLn $ "Preparing to write" <> show (length toWrite) <> " scripts"
  index <- foldM go M.empty (M.toList toWrite)
  putStrLn $ "Writing Index.json"
  writeIndex index
  putStrLn "Done"
  where
    go :: Map Text FilePath -> (Text, TypedScriptEnvelope) -> IO (M.Map Text FilePath)
    go acc (nm, env) = do
      let hash = hashTypedEnvelope env
          hashStr = showHash hash
      putStrLn $ "Writing " <> T.unpack nm <> " to " <> "../compiled-scripts" </> hashStr <> ".plutus"
      writeEnvelope
        ("../compiled-scripts" </> hashStr <> ".plutus")
        env
      pure $ M.insert nm hashStr acc

    deleteOldScripts :: IO ()
    deleteOldScripts = do
      files <- listDirectory "../compiled-scripts"
      traverse_ removeFile files

    writeIndex :: Map Text FilePath -> IO ()
    writeIndex = LBS.writeFile filepath . encodePretty
      where
        filepath = "../compiled-scripts" </> "Index.json"

{- I guess we don't need this here?
runImporter ::
  FilePath -> -- Path to the *directory* that contains the serialized scripts + Index.json
  IO (Map Text TypedScriptEnvelope)
runImporter path =
  eitherDecodeFileStrict (path </> "Index.json") >>= \case
    Left err -> throwIO (userError err)
    Right index -> foldM go M.empty (M.toList index)
  where
    go :: Map Text TypedScriptEnvelope -> (Text, FilePath) -> IO (Map Text TypedScriptEnvelope)
    go acc (nm, file) = do
      env <- readEnvelope (path </> file <> ".plutus")
      pure $ M.insert nm env acc
-}
