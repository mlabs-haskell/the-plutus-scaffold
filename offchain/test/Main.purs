-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.Scaffold.Main (main, suite, showWalletTestContract, mintBurnTestContract, lockUnlockTestContract) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Monad (liftContractM, Contract)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipConfig
  , PlutipTest
  , testPlutipContracts
  , withKeyWallet
  , withWallets
  )
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Validator (spendFromPassword', payToPassword')
import Utils (stringToTokenName)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import NFT (mintTokens', burnTokens')
import Data.BigInt (fromInt) as BigInt
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (group, test)
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

showWalletTestContract :: Contract Unit
showWalletTestContract = do
  logInfo' "Welcome to CTL! Your wallet's payment PubKey hash is:"
  logInfo' <<< show =<< ownPaymentPubKeyHash

mintBurnTestContract :: Contract Unit
mintBurnTestContract = do
  tokenName <- liftContractM "Cannot make token name" $ stringToTokenName "tokenName"
  mintTokens' tokenName (BigInt.fromInt 4)
  burnTokens' tokenName (BigInt.fromInt 4)

lockUnlockTestContract :: Contract Unit
lockUnlockTestContract = do
  password <- liftContractM "Cannot make token name" $ byteArrayFromAscii "password123"
  lockingTxHash <- payToPassword' password (BigInt.fromInt 5)
  spendFromPassword' password lockingTxHash

suite :: TestPlanM PlutipTest Unit
suite =
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  in
    group "Project tests" do
      test "Print PubKey" do
        withWallets distribution \wallet -> do
          withKeyWallet wallet do
            showWalletTestContract
      test "Mint then Burn succeeds" do
        withWallets distribution \wallet -> do
          withKeyWallet wallet do
            mintBurnTestContract
      test "Lock then Unlock succeeds" do
        withWallets distribution \wallet -> do
          withKeyWallet wallet do
            lockUnlockTestContract

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.05 }
  }
