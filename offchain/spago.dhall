{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aeson"
  , "aff"
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
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "profunctor"
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
