{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Plutarch.ExampleContracts (alwaysSucceeds, nftMp, mkPasswordValidator, testPasswordValidator) where

import Plutarch.Api.V1
import Plutarch.Api.V1.Scripts
import qualified Plutarch.Api.V1.Value as PValue
import Plutarch.Builtin (pasByteStr)
import Plutarch.Prelude
import Prelude

-- import Plutarch (compile)

alwaysSucceeds :: Term s PValidator
alwaysSucceeds = plam $ \_ _ _ -> popaque $ pconstant ()

nftMp :: ClosedTerm (PScriptHash :--> PTxOutRef :--> PTokenName :--> PMintingPolicy)
nftMp = plam $ \_ ref tn _ ctx' -> popaque $
  unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- tcont $ pletFields @'["inputs", "mint"] $ getField @"txInfo" ctx
    pguardC "UTxO not consumed" $
      pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $
        getField @"inputs" txInfo
    pguardC "Wrong NFT mint amount" $
      PValue.pvalueOf # getField @"mint" txInfo # ownSym # tn #== 1
    pure $ pconstant ()

pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s

{-
pletFieldsC :: forall t. _ => _
pletFieldsC x = tcont $ pletFields @t x

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch
-}

pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- NOTE: NEVER USE ANYTHING LIKE THIS IN A REAL CONTRACT!!!!!
--       An attacker could almost assuredly determine the password
--       by analyzing the initial transaction that locked funds at the script
--       (I'm only using it here b/c it makes a good nontrivial example)
pmkPasswordValidator ::
  Term
    s
    ( PByteString
        :--> PUnit -- PW needed to unlock
        :--> PByteString
        :--> PScriptContext -- Password (in "plaintext" - bad!)
        :--> POpaque
    )
pmkPasswordValidator = plam $ \pwHash _ pw _ -> unTermCont $ do
  -- maybe add a check that all funds locked at the script are spent?
  pguardC "Incorrect password" $
    (psha3_256 # pw) #== pwHash
  pure . popaque $ pcon PUnit

mkPasswordValidator :: Term s (PByteString :--> PValidator)
mkPasswordValidator = plam $ \pwstr _ pwD cxt -> unTermCont $ do
  pwHash <- pletC $ psha3_256 # pwstr
  pw <- pletC $ pasByteStr # pwD
  mkValidator <- pletC $ pmkPasswordValidator # pwHash
  pure $ mkValidator # pcon PUnit # pw # cxt

testPasswordValidator :: Term s PValidator
testPasswordValidator = mkPasswordValidator # pconstant "password"
