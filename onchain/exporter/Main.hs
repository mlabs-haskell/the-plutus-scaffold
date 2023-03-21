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

import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import Plutarch.ExampleContracts (alwaysSucceeds, nftMp)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

main :: IO ()
main = do
  dir <- head <$> getArgs
  runExporter dir $ do
    hash <- save "always_succeeds" alwaysSucceeds
    save_ "nft_hash_applied" (nftMp # pconstant hash)
    save_ "nft_no_hash_applied" nftMp

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

runExporter :: FilePath -> App () -> IO ()
runExporter dir exporter = do
  createDirectoryIfMissing True dir
  scriptsMap <- either (throwIO . userError . T.unpack) pure $ flip execStateT M.empty exporter
  putStrLn $ "Writing " <> show (length scriptsMap) <> " scripts"
  let scriptsMapWithHashes = M.map (\env -> (env, hashTypedEnvelope env)) scriptsMap
  -- save plutus script files
  traverse_ saveScript $ M.toList scriptsMapWithHashes
  putStrLn $ "Writing Index.json"
  -- save json "name : hash" mapping
  writeIndex $ M.map (showHash . snd) scriptsMapWithHashes
  putStrLn "Done"
  where
    saveScript :: (Text, (TypedScriptEnvelope, ScriptHash)) -> IO ()
    saveScript (nm, (env, hash)) = do
      let hashStr = showHash hash
      putStrLn $ "Writing " <> T.unpack nm <> " to " <> dir </> hashStr <> ".plutus"
      writeEnvelope
        (dir </> hashStr <> ".plutus")
        env

    writeIndex :: Map Text String -> IO ()
    writeIndex = LBS.writeFile filepath . encodePretty
      where
        filepath = dir </> "Index.json"

{- Example importer:
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
