{-# LANGUAGE TemplateHaskell #-}

module Types (MintRedeemer (..)) where

import PlutusLedgerApi.V1
import PlutusTx (makeIsDataIndexed)

data MintRedeemer
  = MintTokens
  | BurnTokens
  deriving (Show, Eq)

makeIsDataIndexed ''MintRedeemer [('MintTokens, 0), ('BurnTokens, 1)]
