{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module EmpAssets where

import           Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            Validator,
                                            mkValidatorScript,
                                            TxInfo (txInfoOutputs), 
                                            TxOut (txOutAddress))
import qualified PlutusTx                  (applyCode, compile, liftCode)
import           PlutusTx.Prelude          (Bool, (.), ($), traceIfFalse, (&&),
                                            Maybe (Just), any, Eq ((==)), error)
import           Prelude                   (IO, String)
import           Text.Printf               (printf)
import           Utilities                 (wrapValidator, writeValidatorToFile)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Address  (toPubKeyHash)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkEmpAssetsValidator #-}
--                      Emp Param     Asset Dept Datum
mkEmpAssetsValidator :: PubKeyHash -> PubKeyHash -> () -> ScriptContext -> Bool
mkEmpAssetsValidator empPkh assetDepPkh () ctx = 
    traceIfFalse "missing dep signature" validDepSig && 
    traceIfFalse "missing emp signature" validEmpSig &&
    traceIfFalse "output must go to dep" validDepOut
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- validate the tx is signed by the asset dept
        validDepSig :: Bool
        validDepSig = txSignedBy txInfo assetDepPkh

        -- validate the tx is signed by the employee
        validEmpSig :: Bool
        validEmpSig = txSignedBy txInfo empPkh

        -- validate the output goes to asset dept
        validDepOut :: Bool
        validDepOut = any outIsDep $ txInfoOutputs txInfo
            where outIsDep = \txOut -> assetDepPkh == outPkh txOut

        -- helper to get PKH out of TxOut
        outPkh :: TxOut -> PubKeyHash
        outPkh txOut = case toPubKeyHash . txOutAddress $ txOut of
            (Just pkh') -> pkh'
            _           -> error ()


{-# INLINABLE  mkWrappedEmpAssetsValidator #-}
mkWrappedEmpAssetsValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEmpAssetsValidator = wrapValidator . mkEmpAssetsValidator

validator :: PubKeyHash -> Validator
validator pkh = mkValidatorScript 
    ($$(PlutusTx.compile [|| mkWrappedEmpAssetsValidator ||]) 
     `PlutusTx.applyCode` 
     PlutusTx.liftCode pkh)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveValidator :: String -> PubKeyHash -> IO ()
saveValidator name pkh = writeValidatorToFile file $ validator pkh
    where file = printf "assets/%s-script.plutus" name
