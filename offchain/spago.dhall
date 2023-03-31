{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "ply-ctl"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "spec"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
