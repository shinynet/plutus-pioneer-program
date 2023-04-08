{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module MintAsset where

import           Plutus.V2.Ledger.Api      (BuiltinData, CurrencySymbol,
                                            MintingPolicy, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo, TxOut, txInfoOutputs,
                                            mkMintingPolicyScript, txOutAddress)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (.), 
                                            Maybe (Just), error, any, (==), (&&))
import           Prelude                   (IO)
import           Utilities                 (currencySymbol, wrapPolicy, writePolicyToFile)
import           Plutus.V1.Ledger.Address  (toPubKeyHash)

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = traceIfFalse "missing signature" validSig 
                   && traceIfFalse "disallowed output" validOut
    -- TODO: while we're checking whether the outputs contain the pkh
    -- we should also check the pkh output is the one with the token
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- validate the tx is signed by the pkh
        validSig :: Bool
        validSig = txSignedBy txInfo pkh

        -- validate the tx has the pkh as the output
        validOut :: Bool
        validOut = any (\txOut -> pkh == outPkh txOut) $ txInfoOutputs txInfo

        outPkh :: TxOut -> PubKeyHash
        outPkh txOut = case toPubKeyHash . txOutAddress $ txOut of
            (Just pkh') -> pkh'
            _           -> error ()


{-# INLINABLE mkWrappedPolicy #-}
mkWrappedPolicy :: PubKeyHash -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy . mkPolicy

signedPolicy :: PubKeyHash -> MintingPolicy
signedPolicy pkh = mkMintingPolicyScript 
    ($$(PlutusTx.compile [|| mkWrappedPolicy ||]) 
    `PlutusTx.applyCode` PlutusTx.liftCode pkh)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

savePolicy :: PubKeyHash -> IO ()
savePolicy pkh = writePolicyToFile "assets/mint-asset.plutus" $ signedPolicy pkh

tokenCurrencySymbol :: PubKeyHash -> CurrencySymbol
tokenCurrencySymbol = currencySymbol . signedPolicy
