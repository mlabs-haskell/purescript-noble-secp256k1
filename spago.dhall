{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "cardano-transaction-lib"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arraybuffer-types"
  , "effect"
  , "maybe"
  , "newtype"
  , "prelude"
  , "spec"
  , "tuples"
  , "unsafe-coerce"
  , "bytearrays"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
