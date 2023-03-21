module MLabsPlutusTemplate.ScriptImports
  ( main
  ) where

import Prelude

import Aeson (decodeAeson, fromString)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Data.Argonaut (Json, parseJson, toObject, toString)
import Data.Array (all)
import Data.Either (Either(..), either, isRight)
import Data.Maybe (Maybe, maybe)
import Data.String (joinWith)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (unicode)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (toUnfoldable)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)

-- TODO: make sure created modules are formatted

ps_module_name :: String
ps_module_name = "MLabsPlutusTemplate.Scripts"

index_file_name :: String
index_file_name = "Index.json"

-- defined by place of use i guess
relative_path_js_output_to_scripts :: String
relative_path_js_output_to_scripts = "../.."

main :: Effect Unit
main = launchAff_ $ do
  args <- liftEffect argv
  case args of
    [ _, scriptsDirpath, modulesDirpath ] -> do
      index <- readIndex (scriptsDirpath <</>> index_file_name)
      if validateIndex index then do
        writeTextFile UTF8 (modulesDirpath <</>> "Scripts.js") $ jsModule scriptsDirpath index
        writeTextFile UTF8 (modulesDirpath <</>> "Scripts.purs") $ psModule (map fst index)
      else
        throwError $ error "Invalid index file: script names should be identifiers and script hashes be hex encoded hashes."
    _ -> throwError $ error "Provide the path to index file and directory path to save generated modules to as two command line argument."

-- Read Index.json file
readIndex ∷ String → Aff (Array (Tuple String String))
readIndex filepath = do
  eindex <- parseJson <$> readTextFile UTF8 filepath
  index <- either (show >>> error >>> throwError) pure eindex
  maybe (throwError $ error "Couldn't decode scripts index.") pure (toKeyValuePairs index)

  where
  toKeyValuePairs :: Json -> Maybe (Array (String /\ String))
  toKeyValuePairs json = do
    pairs <- ((map toString) >>> toUnfoldable) <$> toObject json
    sequence $ (map (case _ of (Tuple x my) -> (Tuple x) <$> my) pairs)

-- Verifies that 
--  1) script names are valid identifiers
--  2) script hashes are valid hashes
validateIndex :: Array (Tuple String String) -> Boolean
validateIndex index = all
  ( \x ->
      case x of
        (Tuple name hash) ->
          test identifier name
            && isRight ((decodeAeson $ fromString hash) :: Either _ ScriptHash)
  )
  index

  where
  identifier = unsafePartial $ case regex "[a-z][A-Za-z0-9_]*" unicode of
    Right reg -> reg

jsModule :: String -> Array (Tuple String String) -> String
jsModule scriptsDirpath index = joinWith "\n" $
  [ nodeImports scriptsDirpath ]
    <> map (uncurry importScriptJS) index

importScriptJS :: String -> String -> String
importScriptJS script_name script_hash =
  ifBrowserRuntime
    (assign script_name (require_browser script_hash))
    (assign script_name (require_node script_hash))
    <>
      export script_name
    <> "\n"

export :: String -> String
export script_name = "exports." <> script_name <> " = " <> script_name <> ";"

-- Expects script_name being valid indentifier, checked by validateIndex
assign :: String -> String -> String
assign script_name value = script_name <> " = " <> value <> ";"

require_node :: String -> String
require_node script_hash = "read_script(\"" <> filename <> "\");"
  where
  filename = script_hash <> ".plutus"

require_browser :: String -> String
require_browser script_hash = "require(\"Scripts/" <> filename <> "\");"
  where
  filename = script_hash <> ".plutus"

ifBrowserRuntime :: String -> String -> String
ifBrowserRuntime thn els =
  "if (typeof BROWSER_RUNTIME != \"undefined\" && BROWSER_RUNTIME) {\n"
    <> thn
    <> "\n} else { \n"
    <> els
    <>
      "\n}\n"

nodeImports :: String -> String
nodeImports scriptsDirpath =
  "let read_script;\n"
    <>
      ifBrowserRuntime "" (nodeImportsSnippet scriptsDirpath)

nodeImportsSnippet :: String -> String
nodeImportsSnippet scriptsDirpath =
  """
  const fs = require("fs");
  const path = require("path");
  read_script = fp => {
    return fs.readFileSync(
      path.resolve(__dirname, 
  """
    <> "\""
    <> relative_path_js_output_to_scripts <</>> scriptsDirpath
    <> "\""
    <>
      """.concat(fp)),
      "utf8"
    );
  };
  """

psModule :: Array String -> String
psModule script_names =
  modulePreamblePS script_names
    <>
      ( joinWith "\n" $
          map importScriptPS script_names
      )

importScriptPS :: String -> String
importScriptPS script_name =
  "foreign import " <> script_name <> " :: String"

modulePreamblePS :: Array String -> String
modulePreamblePS script_names =
  "module " <> ps_module_name <> "\n"
    <> "  ( "
    <>
      joinWith "\n  , " script_names
    <>
      "\n  ) where\n\n"

-- TODO: remove if not useful
-- decodePlutusScript = do
--   env <- decodeTextEnvelope
--   case env.type_ of 
--     PlutusScriptV1 -> Just $ plutusV1Script env.bytes
--     PlutusScriptV2 -> Just $ plutusV2Script env.bytes
--     PaymentSigningKeyShelleyed25519 -> Nothing
--     StakeSigningKeyShelleyed25519 -> Nothing
--     Other other -> Nothing
