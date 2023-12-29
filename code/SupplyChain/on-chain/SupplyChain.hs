{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module SupplyChain where

import Plutus.V2.Ledger.Api     ( POSIXTime,
                                   BuiltinByteString,
                                   Datum(Datum),
                                   OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
                                   ScriptContext(scriptContextTxInfo),
                                   TxInInfo(txInInfoResolved),
                                   TxOut(txOutDatum, txOutValue),
                                   BuiltinData,
                                   PubKeyHash,
                                   TxInfo,
                                   txOutAddress,
                                    UnsafeFromData (unsafeFromBuiltinData), Validator ,mkValidatorScript, ToData (toBuiltinData))
import Plutus.V1.Ledger.Value    ( assetClassValueOf, AssetClass(AssetClass, unAssetClass))
import Plutus.V2.Ledger.Contexts ( findDatum, txSignedBy  , getContinuingOutputs , findOwnInput)
import PlutusTx                  (  unstableMakeIsData, FromData(fromBuiltinData) , CompiledCode , compile, applyCode, liftCode)
import PlutusTx.Prelude          ( Bool(..),
                                   Maybe(..),
                                   (/=),
                                   traceError,
                                   (&&),
                                   traceIfFalse,
                                   ($),
                                   (.),
                                   Eq(..)  )
import           Utilities        (wrapValidator)
import           Prelude                    (IO , Show())
import           Utilities            ( writeCodeToFile)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseSupplyChainDatum #-}
parseSupplyChainDatum :: OutputDatum -> TxInfo -> Maybe SupplyChainDatum
parseSupplyChainDatum o info = case o of
    NoOutputDatum         -> traceError "Found Collateral output but NoOutputDatum"
    OutputDatum (Datum d) -> fromBuiltinData d
    OutputDatumHash dh    -> do
                           Datum d <- findDatum dh info
                           fromBuiltinData d

{-# INLINABLE removeLast #-}
removeLast :: [a] -> [a]
removeLast [] = [] 
removeLast [_] = [] 
removeLast (x:xs) = x : removeLast xs 

{-# INLINABLE retrieveLast #-}
retrieveLast :: [a] -> a
retrieveLast [] = traceError "Not the Empty Array"
retrieveLast [a] = a
retrieveLast (_:xs) = retrieveLast xs

data SupplyChainRedemer = Comments | Transfer deriving Show

data SupplyChainDatum = SupplyChainDatum {
    manufacturer :: PubKeyHash,
    manufactuerDate :: POSIXTime,
    expiryDate :: POSIXTime ,
    photos :: [BuiltinByteString],
    currentOwner :: PubKeyHash,
    owners::[PubKeyHash],
    comments::[BuiltinByteString]
} deriving Show

unstableMakeIsData '' SupplyChainDatum
unstableMakeIsData '' SupplyChainRedemer

supplyChainValidator :: AssetClass ->  SupplyChainDatum -> SupplyChainRedemer -> ScriptContext -> Bool
supplyChainValidator assetId dtm r ctx  =
 case r of 
    Comments ->              traceIfFalse "Should be signed by Current Owner" checkCurrentOwnerSignature &&
                             traceIfFalse "Only Comments should change" checkOnlyCommentChanges  && 
                             traceIfFalse "Check Nft in input"  inputHasToken && 
                             traceIfFalse "Check Nft in Output" outputHasToken &&
                             traceIfFalse "Check Output Adddress Same as Current Address" checkOutPutAddress
    Transfer ->              traceIfFalse "Should be signed by Current Owner" checkCurrentOwnerSignature &&
                             traceIfFalse "Only Owner and CurrentOwner should change" checkOwnersAndCurrentOwnerChanges  && 
                             traceIfFalse "Check Nft in input"  inputHasToken && 
                             traceIfFalse "Check Nft in Output" outputHasToken &&
                             traceIfFalse "Check Output Adddress Same as Current Address" checkOutPutAddress
    where
            info :: TxInfo
            info = scriptContextTxInfo ctx
            
            checkCurrentOwnerSignature :: Bool
            checkCurrentOwnerSignature = txSignedBy  info $ currentOwner dtm

            ownOutput ::TxOut
            ownOutput = case getContinuingOutputs ctx of 
                        [o]->o        
                        _ -> traceError "Only one output expected"

            ownInput :: TxOut
            ownInput = case findOwnInput ctx of 
                      Nothing -> traceError "Only one output expected"
                      Just i ->  txInInfoResolved i       

            inputHasToken :: Bool
            inputHasToken = assetClassValueOf (txOutValue ownInput) assetId == 1 

            outputHasToken :: Bool
            outputHasToken = assetClassValueOf (txOutValue ownOutput) assetId == 1

            outputDatums ::   SupplyChainDatum
            outputDatums = case  parseSupplyChainDatum (txOutDatum ownOutput) info of 
                           Just i -> i
           
            checkOutPutAddress::Bool
            checkOutPutAddress = txOutAddress ownInput ==  txOutAddress  ownOutput

            checkOnlyCommentChanges :: Bool
            checkOnlyCommentChanges = traceIfFalse "Plz write the correct datum" $ manufacturer dtm == manufacturer outputDatums &&
                                                                                   manufactuerDate dtm == manufactuerDate outputDatums &&
                                                                                   expiryDate dtm == expiryDate outputDatums &&
                                                                                   photos dtm == photos outputDatums &&
                                                                                   currentOwner dtm == currentOwner outputDatums &&
                                                                                   owners dtm == owners outputDatums && 
                                                                                   checkComment

            checkComment :: Bool
            checkComment = comments dtm == removeLast (comments outputDatums)                                                                        

            checkOwnersAndCurrentOwnerChanges :: Bool
            checkOwnersAndCurrentOwnerChanges = traceIfFalse "Plz write the correct datum" $ manufacturer dtm == manufacturer outputDatums &&
                                                                                            manufactuerDate dtm == manufactuerDate outputDatums &&
                                                                                            expiryDate dtm == expiryDate outputDatums &&
                                                                                            photos dtm == photos outputDatums &&
                                                                                            currentOwner dtm /= currentOwner outputDatums &&
                                                                                            currentOwner outputDatums == retrieveLast (owners outputDatums) && 
                                                                                            comments dtm == comments outputDatums &&
                                                                                            checkOwners
            
            checkOwners :: Bool
            checkOwners = removeLast (owners outputDatums) == owners dtm


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator cs tn = wrapValidator . supplyChainValidator $ AssetClass  (unsafeFromBuiltinData cs, unsafeFromBuiltinData tn)
   
supplyChainCodeWithOutParams :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData  -> BuiltinData -> BuiltinData -> BuiltinData -> ())
supplyChainCodeWithOutParams = $$(compile [|| mkWrappedValidator ||])

supplyChainCodeWithParams :: AssetClass -> Validator
supplyChainCodeWithParams assets = mkValidatorScript   $ supplyChainCodeWithOutParams  `applyCode` liftCode   (toBuiltinData    x)   `applyCode` liftCode  ( toBuiltinData  y)
                              where
                                (x,y) = unAssetClass assets

saveSupplyChainCode :: IO ()
saveSupplyChainCode = writeCodeToFile "assets/supplyChain.plutus" supplyChainCodeWithOutParams
     