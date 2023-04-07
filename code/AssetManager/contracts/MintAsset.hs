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
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (.), Maybe (Just), error, any, (==), (&&))
import           Prelude                   (IO)
import           Utilities                 (currencySymbol, wrapPolicy,
                                            writeCodeToFile, writePolicyToFile)
import Plutus.V1.Ledger.Address ( toPubKeyHash )

{-# INLINABLE mkSignedPolicy #-}
mkSignedPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkSignedPolicy pkh () ctx = traceIfFalse "missing signature" validSig 
                         && traceIfFalse "disallowed output" validOut
    -- TODO: while we're checking whether the outputs contain the pkh
    -- we should also check the pkh output is the one with the token
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- validate the tx is signed by the pkh
        validSig :: Bool
        validSig = txSignedBy txInfo pkh

        outPkh :: TxOut -> PubKeyHash
        outPkh txOut = case toPubKeyHash . txOutAddress $ txOut of
            (Just pkh') -> pkh'
            _           -> error ()

        -- validate the tx has the pkh as the output
        validOut :: Bool
        validOut = any (\txOut -> pkh == outPkh txOut) $ txInfoOutputs txInfo


{-# INLINABLE mkWrappedSignedPolicy #-}
mkWrappedSignedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedSignedPolicy pkh = wrapPolicy (mkSignedPolicy $ PlutusTx.unsafeFromBuiltinData pkh)

signedCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
signedCode = $$(PlutusTx.compile [|| mkWrappedSignedPolicy ||])

signedPolicy :: PubKeyHash -> MintingPolicy
signedPolicy pkh = mkMintingPolicyScript $ signedCode `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveSignedCode :: IO ()
saveSignedCode = writeCodeToFile "assets/signed.plutus" signedCode

saveSignedPolicy :: PubKeyHash -> IO ()
saveSignedPolicy pkh = writePolicyToFile "assets/signed.plutus" $ signedPolicy pkh

signedCurrencySymbol :: PubKeyHash -> CurrencySymbol
signedCurrencySymbol = currencySymbol . signedPolicy
