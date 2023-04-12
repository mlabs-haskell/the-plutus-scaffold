{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Plutarch.ExampleContracts (alwaysSucceeds, nftMp, mkPasswordValidator, mkSimpleMP) where

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

pletFieldsC :: forall t. _ => _
pletFieldsC x = tcont $ pletFields @t x

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

(#>) :: PPartialOrd t => Term s t -> Term s t -> Term s PBool
p1 #> p2 = pnot # (p1 #<= p2)
infix 4 #>

data PMintRedeemer (s :: S)
  = PMintTokens (Term s (PDataRecord '[]))
  | PBurnTokens (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PTryFrom PData)
instance DerivePlutusType PMintRedeemer where type DPTStrat _ = PlutusTypeData

pmkSimpleMP :: ClosedTerm (PAsData PTokenName :--> PAsData PMintRedeemer :--> PScriptContext :--> POpaque)
pmkSimpleMP = plam $ \tn redeemer ctx' -> popaque $ unTermCont $ do
  ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
  PMinting mintFlds <- pmatchC $ getField @"purpose" ctx
  let ownSym = pfield @"_0" # mintFlds
  txInfo <- tcont $ pletFields @'["inputs", "mint"] $ getField @"txInfo" ctx
  pmatchC (pfromData redeemer) >>= \case
    PMintTokens _ -> do
      pguardC "Tokens minted <= 0 with 'MintTokens' redeemer" $
        (PValue.pvalueOf # txInfo.mint # ownSym # pfromData tn) #> 0
    PBurnTokens _ -> do
      pguardC "Tokens minted >= 0 with 'BurnTokens' redeemer" $
        (PValue.pvalueOf # txInfo.mint # ownSym # pfromData tn) #< 0
  pure $ pcon PUnit

mkSimpleMP :: ClosedTerm (PAsData PTokenName :--> PMintingPolicy)
mkSimpleMP = plam $ \tn redeemerD ctx ->
  pmkSimpleMP # tn # ptryFrom redeemerD (pdata . fst) # ctx

-- NOTE: NEVER USE ANYTHING LIKE THIS IN A REAL CONTRACT!!!!!
--       An attacker could almost assuredly determine the password
--       if they have access to the serialized script
--       (I'm only using it here b/c it makes a good nontrivial example)
pmkPasswordValidator ::
  Term
    s
    ( PByteString
        :--> PUnit
        :--> PByteString  -- Password in "plaintext" - bad!
        :--> PScriptContext
        :--> POpaque
    )
pmkPasswordValidator = plam $ \pwHash _ pw _ -> unTermCont $ do
  pguardC "Incorrect password" $
    (psha3_256 # pw) #== pwHash
  pure . popaque $ pcon PUnit

mkPasswordValidator :: Term s (PAsData PByteString :--> PValidator)
mkPasswordValidator = plam $ \pwstr _ pwD cxt -> unTermCont $ do
  pwHash <- pletC $ psha3_256 # pfromData pwstr
  pw <- pletC $ pasByteStr # pwD
  mkValidator <- pletC $ pmkPasswordValidator # pwHash
  pure $ mkValidator # pcon PUnit # pw # cxt
