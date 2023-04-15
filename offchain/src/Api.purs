module MlabsPlutusTemplate.Api
  ( module Validator
  , module NFT
  , module Config
  , stringToTokenNameJS
  , stringToPosBigIntJS
  , passwordFromAsciiJS
  ) where

import Prelude ((<<<), ($), (>=))
import Data.Maybe (Maybe(Nothing, Just))
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.Value (TokenName)
import Contract.Config
  ( testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , testnetNuFiConfig
  ) as Config

import Data.BigInt (BigInt)
import Data.BigInt (fromString, fromInt) as BigInt
import Data.Nullable (Nullable, toNullable)

import Data.Function.Uncurried (Fn1, mkFn1)

import Validator (payToPassword, spendFromPassword)
import NFT (mintTokens, burnTokens)

import Utils

-- Validation functions. Easier to write here than in JS

-- Presumably there is a way to unwrap a PS Maybe in JS, but
-- I can't figure it out, so we use this
stringToTokenNameJS :: Fn1 String (Nullable TokenName)
stringToTokenNameJS = mkFn1 $ \str -> toNullable <<< stringToTokenName $ str

stringToPosBigIntJS :: Fn1 String (Nullable BigInt)
stringToPosBigIntJS = mkFn1 $ \str -> toNullable $ case BigInt.fromString str of
  Nothing -> Nothing
  Just bi -> if bi >= (BigInt.fromInt 0) then Just bi else Nothing

passwordFromAsciiJS :: Fn1 String (Nullable ByteArray)
passwordFromAsciiJS = mkFn1 $ \str -> toNullable (byteArrayFromAscii str)
