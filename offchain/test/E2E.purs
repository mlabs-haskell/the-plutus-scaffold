-- | This module is used to serve the E2E tests to the headless browser.
module Test.E2E.Serve where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Config (ContractParams, mainnetFlintConfig, mainnetGeroConfig, mainnetLodeConfig, mainnetNamiConfig, testnetEternlConfig, testnetFlintConfig, testnetGeroConfig, testnetLodeConfig, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Test.Cip30Mock (WalletMock(MockFlint, MockGero, MockNami, MockLode))
import Contract.Test.E2E (E2EConfigName, E2ETestName, addLinks, route)
import Test.Scaffold.Main (showWalletTestContract, mintBurnTestContract, lockUnlockTestContract)
import Data.Map (Map)
import Data.Map as Map

main :: Effect Unit
main = do
  addLinks configs tests
  route configs tests

configs :: Map E2EConfigName (ContractParams /\ Maybe WalletMock)
configs = Map.fromFoldable
  [ "nami" /\ testnetNamiConfig /\ Nothing
  , "gero" /\ testnetGeroConfig /\ Nothing
  , "flint" /\ testnetFlintConfig /\ Nothing
  , "eternl" /\ testnetEternlConfig /\ Nothing
  , "lode" /\ testnetLodeConfig /\ Nothing
  , "nami-mock" /\ testnetNamiConfig /\ Just MockNami
  , "gero-mock" /\ testnetGeroConfig /\ Just MockGero
  , "flint-mock" /\ testnetFlintConfig /\ Just MockFlint
  , "lode-mock" /\ testnetLodeConfig /\ Just MockLode
  -- Plutip cluster's network ID is set to mainnet:
  , "plutip-nami-mock" /\ mainnetNamiConfig /\ Just MockNami
  , "plutip-gero-mock" /\ mainnetGeroConfig /\ Just MockGero
  , "plutip-flint-mock" /\ mainnetFlintConfig /\ Just MockFlint
  , "plutip-lode-mock" /\ mainnetLodeConfig /\ Just MockLode
  ]

tests :: Map E2ETestName (Contract Unit)
tests = Map.fromFoldable
  -- TAG: E2E-CONTRACT-NAMES
  [ "Print-PubKey" /\ showWalletTestContract
  , "Mint-Burn" /\ mintBurnTestContract
  , "Lock-Unlock" /\ lockUnlockTestContract
  ]
