{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified NFT
import qualified SupplyChain
import           Control.Monad          (replicateM, unless, Monad (return), void , mapM)
import           Plutus.Model           (Ada (Lovelace), DatumMode (..),
                                         Run, Tx, TypedValidator (TypedValidator),
                                         UserSpend, ada, adaValue,
                                         defaultBabbage, logError, mustFail,
                                         newUser, payToKey, payToScript, spend, submitTx, testNoErrors,
                                         toV2, userSpend, utxoAt,
                                         valueAt, TypedPolicy (TypedPolicy), mintValue, spendPubKey, scriptCurrencySymbol,
                                          spendScript)
import           Plutus.V2.Ledger.Api   (PubKeyHash,
                                         TxOut (txOutValue), TxOutRef, Value, singleton,
                                         TokenName,
                                          POSIXTime, BuiltinByteString)
import           PlutusTx.Prelude       (Eq ((==)), ($), (.), Bool (True, False))
import           Prelude                (IO, mconcat, Semigroup ((<>)))
import           Test.Tasty             (defaultMain, testGroup)
import           Plutus.V1.Ledger.Value (assetClass, AssetClass (), assetClassValue)
import SupplyChain (SupplyChainDatum)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------
main :: IO ()
main = defaultMain $ do
    testGroup
      "Test simple user transactions"
      [ good "-Successful minting of an NFT"      testMintNFT ,
        bad  "-Attempting to mint the same NFT twice (expected failure)" testMintNFTTwice,
        bad  "-Transfer attempted by someone other than the current owner (expected failure)" testTransferByOtherIncorrectOwner,
        bad  "-Comment attempted by someone other than the current owner (expected failure)"  testCommentsByOtherIncorrectOwner,
         bad  "-Comment attempted to someone other than the current validater (expected failure)" $ testTransferAndCommentOfProductToOtherAdddress SupplyChain.Transfer  100 300 ["Photo1" ,"Photo2"] [],
         bad  "-Comment attempted to someone other than the current validater (expected failure)" $ testTransferAndCommentOfProductToOtherAdddress SupplyChain.Comments  100 300 ["Photo1" ,"Photo2"] ["The product is really good"],
        good "-Successful transfer with correct datum" $ testTransferAndCommentOfProductWithDatum SupplyChain.Transfer  100 300 ["Photo1" ,"Photo2"] [],
        good "-Successful comment with correct datum" $  testTransferAndCommentOfProductWithDatum SupplyChain.Comments  100 300 ["Photo1", "Photo2"] ["The product is really cool"],
        bad  "-Attempted comment with incorrect datum (expected failure) : Manufacture Date" $  testTransferAndCommentOfProductWithDatum SupplyChain.Comments  200 300 ["Photo1", "Photo2"] ["The product is really cool"],
        bad  "-Attempted comment with incorrect datum (expected failure) : ExpiryDate" $  testTransferAndCommentOfProductWithDatum SupplyChain.Comments  100 200 ["Photo1", "Photo2"] ["The product is really cool"],
        bad  "-Attempted comment with incorrect datum (expected failure) : Photo"  $ testTransferAndCommentOfProductWithDatum SupplyChain.Comments  100 300 ["Photo1"] ["The product is really cool"],
        bad  "-Attempted comment with incorrect datum (expected failure) : Commemnts" $  testTransferAndCommentOfProductWithDatum SupplyChain.Comments  100 300 ["Photo1 , Photo2"] [],
        bad  "-Attempted comment with incorrect datum (expected failure) : Manufacturer" $  testCommentAndtransferOfProductWithCorrectIncorrectDatumWithUser SupplyChain.Comments True,
        bad  "-Attempted comment with incorrect datum (expected failure) : Owners"  $ testCommentAndtransferOfProductWithCorrectIncorrectDatumWithUser SupplyChain.Comments False,
        bad  "-Attempted transfer with incorrect datum (expected failure) : Manufacture Date" $   testTransferAndCommentOfProductWithDatum SupplyChain.Transfer  200 300 ["Photo1", "Photo2"] [],
        bad  "-Attempted transfer with incorrect datum (expected failure) : ExpiryDate"  $ testTransferAndCommentOfProductWithDatum SupplyChain.Transfer  100 200 ["Photo1", "Photo2"] [],
        bad  "-Attempted transfer with incorrect datum (expected failure) : Photo"  $ testTransferAndCommentOfProductWithDatum SupplyChain.Transfer  100 300 ["Photo1"] [],
        bad  "-Attempted transfer with incorrect datum (expected failure) : Comments" $  testTransferAndCommentOfProductWithDatum SupplyChain.Transfer  100 300 ["Photo1 , Photo2"] ["The product is really cool"],
        bad  "-Attempted transfer with incorrect datum (expected failure) : Manufacturer" $  testCommentAndtransferOfProductWithCorrectIncorrectDatumWithUser SupplyChain.Transfer True,
        bad  "-Attempted Transfer with incorrect datum (expected failure) : Owners" $  testCommentAndtransferOfProductWithCorrectIncorrectDatumWithUser SupplyChain.Transfer False
            ]      where
        bad msg = good msg . mustFail
        good = testNoErrors (adaValue 10_000_000_000) defaultBabbage


type SupplyChainScript = TypedValidator SupplyChain.SupplyChainDatum SupplyChain.SupplyChainRedemer

instance Eq SupplyChain.SupplyChainRedemer where
  SupplyChain.Comments == SupplyChain.Comments = True
  SupplyChain.Transfer == SupplyChain.Transfer = True
  _ == _ = False

supplyChainScript :: AssetClass -> SupplyChainScript
supplyChainScript x = TypedValidator . toV2 $ SupplyChain.supplyChainCodeWithParams x
-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1_000_000_000)

-- NFT Minting Policy's script
nftScript :: TxOutRef -> TokenName -> TypedPolicy ()
nftScript ref tn = TypedPolicy . toV2 $ NFT.nftPolicy ref tn

firstSuppyChainDatum :: PubKeyHash -> SupplyChain.SupplyChainDatum
firstSuppyChainDatum u1 = SupplyChain.SupplyChainDatum u1 100 300 ["Photo1", "Photo2"] u1 [u1] []

mintNFTTx :: TxOutRef -> TxOut -> TokenName -> Value -> PubKeyHash -> Tx
mintNFTTx ref out tn val pkh =
  mconcat
    [ mintValue (nftScript ref tn) () val
    , payToKey pkh $ val <> txOutValue out
    , spendPubKey ref
    ]

mintNFT :: PubKeyHash -> Run AssetClass
mintNFT u = do
  utxos <- utxoAt u
  let [(ref, out)] = utxos
      currSymbol = scriptCurrencySymbol (nftScript ref "NFT")
      mintingValue = singleton currSymbol "NFT" 1
  submitTx u $ mintNFTTx ref out "NFT" mintingValue u
  v1<- valueAt u
  unless (v1 == adaValue 1000000000 <> mintingValue) $
    logError "Final balances are incorrect"
  return $ assetClass currSymbol "NFT"

testMintNFT :: Run ()
testMintNFT = do
  [u1,_] <- setupUsers
  void $ mintNFT u1

testMintNFTTwice :: Run ()
testMintNFTTwice = do
  [u1,_] <- setupUsers
  utxos <- utxoAt u1
  let [(ref, out)] = utxos
      mintingValue = singleton (scriptCurrencySymbol (nftScript ref "NFT")) "NFT" 1
      tx = mintNFTTx ref out "NFT" mintingValue u1
  submitTx u1 tx
  submitTx u1 tx
  v1 <- valueAt u1
  unless (v1 == adaValue 1000000000 <> mintingValue) $
    logError "Final balances are incorrect"

consumingTx :: PubKeyHash->  SupplyChainScript -> SupplyChain.SupplyChainRedemer -> PubKeyHash -> POSIXTime ->POSIXTime -> [BuiltinByteString]-> PubKeyHash -> [PubKeyHash] -> [BuiltinByteString] -> TxOutRef -> Value -> Tx
consumingTx u1 script red man manDay expiry photos currentOwner owners comments ref val =
  mconcat
    [ spendScript script ref red (firstSuppyChainDatum u1) ,
      payToScript script (HashDatum (SupplyChain.SupplyChainDatum man manDay expiry photos currentOwner owners comments )) val
    ]

lockingTx :: PubKeyHash -> SupplyChainScript  ->UserSpend -> Value -> Tx
lockingTx u1 script   usp val  =
  mconcat
    [ userSpend usp
    , payToScript script (HashDatum (firstSuppyChainDatum u1)) val
    ]

consumingTxToOtherAddress :: PubKeyHash->  SupplyChainScript -> SupplyChain.SupplyChainRedemer ->  TxOutRef -> Value -> Tx
consumingTxToOtherAddress u1 script red ref val =
  mconcat
    [ spendScript script ref red (firstSuppyChainDatum u1) ,
      payToKey  u1 val
    ]

testTransferAndCommentOfProductToOtherAdddress:: SupplyChain.SupplyChainRedemer-> POSIXTime -> POSIXTime -> [BuiltinByteString] -> [BuiltinByteString]-> Run ()
testTransferAndCommentOfProductToOtherAdddress red manufactureDate expiryDate  photos comments = do
  [u1, u2] <- setupUsers
  assetclass <- mintNFT u1
  let val =  assetClassValue  assetclass 1
  sp <- spend u1 val
  let script = supplyChainScript assetclass
      tx = lockingTx u1 script  sp val
  submitTx u1 tx
  utxos <- utxoAt script
  let [(ref, out)] = utxos
  let tx' =  consumingTxToOtherAddress u1 script red ref (txOutValue out) 
  submitTx u1 tx'
  [v1] <- mapM valueAt [u1]
  unless (v1 == adaValue 1_000_000_000) $
     logError "Datum doesn't match!"

testTransferByOtherIncorrectOwner :: Run ()
testTransferByOtherIncorrectOwner = do
  [u1, u2] <- setupUsers
  assetclass <- mintNFT u1
  let val =  assetClassValue  assetclass 1
  sp <- spend u1 val
  let script = supplyChainScript assetclass
      tx = lockingTx u1 script  sp val
  submitTx u1 tx
  utxos <- utxoAt script
  let [(ref, out)] = utxos
  let tx' = consumingTx u1 script  SupplyChain.Transfer u1 100 300 ["Photo1", "Photo2"] u2 [u1 , u2] [] ref (txOutValue out)
  submitTx u2 tx'

testCommentsByOtherIncorrectOwner :: Run ()
testCommentsByOtherIncorrectOwner = do
  [u1, u2] <- setupUsers
  assetclass <- mintNFT u1
  let val =  assetClassValue  assetclass 1
  sp <- spend u1 val
  let script = supplyChainScript assetclass
      tx = lockingTx u1 script  sp val
  submitTx u1 tx
  utxos <- utxoAt script
  let [(ref, out)] = utxos
  let tx' = consumingTx u1 script SupplyChain.Comments u1 100 300 ["Photo1", "Photo2"] u1 [u1] ["The product Is Good"] ref (txOutValue out)
  submitTx u2 tx'

testTransferAndCommentOfProductWithDatum:: SupplyChain.SupplyChainRedemer-> POSIXTime -> POSIXTime -> [BuiltinByteString] -> [BuiltinByteString]-> Run ()
testTransferAndCommentOfProductWithDatum red manufactureDate expiryDate  photos comments = do
  [u1, u2] <- setupUsers
  assetclass <- mintNFT u1
  let val =  assetClassValue  assetclass 1
  sp <- spend u1 val
  let script = supplyChainScript assetclass
      tx = lockingTx u1 script  sp val
  submitTx u1 tx
  utxos <- utxoAt script
  let [(ref, out)] = utxos
  let tx' = if red == SupplyChain.Transfer  then consumingTx u1 script red u1 manufactureDate expiryDate photos u2 [u1 , u2] comments ref (txOutValue out) else consumingTx u1 script red u1 manufactureDate expiryDate photos u1 [u1] comments ref (txOutValue out)
  submitTx u1 tx'
  [v1] <- mapM valueAt [u1]
  unless (v1 == adaValue 1_000_000_000) $
     logError "Datum doesn't match!"

testCommentAndtransferOfProductWithCorrectIncorrectDatumWithUser :: SupplyChain.SupplyChainRedemer -> Bool-> Run()
testCommentAndtransferOfProductWithCorrectIncorrectDatumWithUser red forManufacturer = do
  [u1, u2] <- setupUsers
  assetclass <- mintNFT u1
  let val =  assetClassValue  assetclass 1
  sp <- spend u1 val
  let script = supplyChainScript assetclass
      tx = lockingTx u1 script  sp val
  submitTx u1 tx
  utxos <- utxoAt script
  let [(ref, out)] = utxos
  let tx' = if forManufacturer then if red ==SupplyChain.Transfer then consumingTx u1 script red u1 100 300 ["Photo1 , Photo2"] u2 [u1] [] ref (txOutValue out) else consumingTx u1 script red u1 100 300 ["Photo1 , Photo2"] u2 [u1] ["The product Is good"] ref (txOutValue out) 
                 else if  red ==SupplyChain.Transfer then  consumingTx u1 script red u1 100 300 ["Photo1 , Photo2"] u1 [u1 , u2] [] ref (txOutValue out) else  consumingTx u1 script red u1 100 300 ["Photo1 , Photo2"] u1 [u1 , u2] ["The product is good"] ref (txOutValue out)
  submitTx u1 tx'