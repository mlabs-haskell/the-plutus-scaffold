-- Convinience executable generating purescript modules importing scripts from the output of script exporter.
-- Provide command line arguments:
-- `spago run --main ScriptImports -b <inScriptsDirectory> -b <outModulesDirectory> -b <outModuleName>`
-- where:
--   <inScriptsDirectory>: directory of compiled onchain scripts (output directory of script exporter)
--   <outModulesDirectory>: directory to save generated purescript modules to
--   <outModuleName>: module name for the generated purescript module
module ScriptImports
  ( main
  , scriptDefinition
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
import Node.Path (relative)
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)

index_file_name :: String
index_file_name = "Index.json"

main :: Effect Unit
main =
  launchAff_
    $ do
        args <- liftEffect argv
        case args of
          [ _, scriptsDirpath, modulesDirpath, ps_module_name ] -> do
            index <- readIndex (scriptsDirpath <</>> index_file_name)
            if validateIndex index then do
              writeTextFile UTF8 (modulesDirpath <</>> "Scripts.js") $ jsModule modulesDirpath scriptsDirpath index
              writeTextFile UTF8 (modulesDirpath <</>> "Scripts.purs") $ psModule ps_module_name (map fst index)
            else
              throwError $ error "Invalid index file: script names should be identifiers and script hashes be hex encoded hashes."
          _ ->
            throwError
              $ error
                  """
            Provide command line arguments:
            `spago run --main MLabsPlutusTemplate.ScriptImports -b <inScriptsDirectory> <outModulesDirectory> <outModuleName>`
            where:
              <inScriptsDirectory>: directory of compiled onchain scripts (output directory of script exporter)
              <outModulesDirectory>: directory to save generated purescript modules to
              <outModuleName>: module name for the generated purescript module
            """

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
validateIndex index =
  all
    ( \x -> case x of
        (Tuple name hash) ->
          test identifier name
            && isRight ((decodeAeson $ fromString hash) :: Either _ ScriptHash)
    )
    index
  where
  identifier =
    unsafePartial
      $ case regex "[a-z][A-Za-z0-9_]*" unicode of
          Right reg -> reg

{-
  Js module generation
-}
jsModule :: String -> String -> Array (Tuple String String) -> String
jsModule modulesDirpath scriptsDirpath index =
  joinWith "\n"
    $ [ nodeImports modulesDirpath scriptsDirpath ]
    <> map (uncurry importScriptJS) index

-- if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
-- script_name = require("Scripts/script_hash.plutus");;
-- } else { 
-- script_name = read_script("script_hash.plutus");;
-- }
-- exports.script_name = script_name;
importScriptJS :: String -> String -> String
importScriptJS script_name script_hash =
  ifBrowserRuntime
    (assign script_envelope_name (require_browser script_hash))
    (assign script_envelope_name (require_node script_hash))
    <> export script_name
    <> "\n"
  where
  script_envelope_name = scriptEnvelopeName script_name

-- exports.script_name = script_name;
export :: String -> String
export script_name = assign ("exports." <> script_envelope_name) script_envelope_name
  where
  script_envelope_name = scriptEnvelopeName script_name

-- Expects script_name being valid indentifier, checked by validateIndex
assign :: String -> String -> String
assign script_name value = script_name <> " = " <> value <> ";"

-- script_name = read_script("script_hash.plutus");;
require_node :: String -> String
require_node script_hash = "read_script(\"" <> filename <> "\");"
  where
  filename = script_hash <> ".plutus"

-- script_name = require("Scripts/script_hash.plutus");;
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
    <> "\n}\n"

nodeImports :: String -> String -> String
nodeImports modulesDirpath scriptsDirpath =
  "let read_script;\n"
    <> ifBrowserRuntime "" (nodeImportsSnippet modulesDirpath scriptsDirpath)

nodeImportsSnippet :: String -> String -> String
nodeImportsSnippet modulesDirpath scriptsDirpath =
  """
  const fs = require("fs");
  const path = require("path");
  read_script = fp => {
    return fs.readFileSync(
      path.resolve(__dirname, 
  """
    <> "\""
    -- relative path to scripts
    
    <> relative modulesDirpath scriptsDirpath
    <> "\""
    <> """.concat(fp)),
      "utf8"
    );
  };
  """

-- script_name_envelope
scriptEnvelopeName ∷ String → String
scriptEnvelopeName script_name = script_name <> "_envelope"

{-
  Ps module generation
-}
psModule :: String -> Array String -> String
psModule ps_module_name script_names =
  modulePreamblePS ps_module_name script_names
    <\\> ( joinWith "\n"
          $ map importScriptPS script_names
      )
    <\\> ""

-- foreign import always_succeeds_envelope :: String
-- always_succeeds :: Maybe PlutusScript
-- always_succeeds = parseScript always_succeeds_envelope
importScriptPS :: String -> String
importScriptPS script_name =
  "foreign import " <> psTypeAnnotation (scriptEnvelopeName script_name) "String"
    <\\> scriptDefinition script_name

-- always_succeeds :: Maybe PlutusScript
-- always_succeeds = parseScript always_succeeds_envelope
scriptDefinition :: String -> String
scriptDefinition script_name =
  psTypeAnnotation script_name "Maybe PlutusScript"
    <\\> script_name
    <> " = "
    <> ("parseScript " <> scriptEnvelopeName script_name)

-- module Scripts
-- ( always_succeeds
-- , always_succeeds_envelope
-- ) where
-- 
-- Import Bla
-- ...
-- 
-- parseScript :: String -> Maybe PlutusScript
-- parseScript = ...
modulePreamblePS :: String -> Array String -> String
modulePreamblePS ps_module_name script_names =
  "module " <> ps_module_name
    <\\> "  ( "
    <> joinWith "\n  , " (script_names <> map scriptEnvelopeName script_names)
    <> "\n  ) where\n\n"
    <\\> psImports
    <> parseScriptDefinition

psImports :: String
psImports =
  """
import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap,unwrap)
import Contract.TextEnvelope
import Ctl.Internal.Types.Scripts (PlutusScript)
  """

parseScriptDefinition :: String
parseScriptDefinition =
  """
parseScript :: String -> Maybe PlutusScript
parseScript myscript = case unwrap <$> decodeTextEnvelope myscript of
  Just e -> case e.type_ of
    PlutusScriptV1 -> plutusScriptV1FromEnvelope (wrap e)
    PlutusScriptV2 -> plutusScriptV2FromEnvelope (wrap e)
    _              -> Nothing
  Nothing -> Nothing
"""

concatLines :: String -> String -> String
concatLines a b = a <> "\n" <> b

infixr 5 concatLines as <\\>

-- def :: typename
psTypeAnnotation ∷ String → String → String
psTypeAnnotation def typename = def <> " :: " <> typename
