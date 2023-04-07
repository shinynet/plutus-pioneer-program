{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module EmpAssets where

import           Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo, Validator, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx                  (applyCode, compile, liftCode)
import           PlutusTx.Prelude          (Bool (True), traceIfFalse, (.), ($))
import           Prelude                   (IO, String)
import           Text.Printf               (printf)
import           Utilities                 (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkEmpAssetsValidator #-}
mkEmpAssetsValidator :: PubKeyHash -> () -> () -> ScriptContext -> Bool
mkEmpAssetsValidator _ () () _ = True
-- mkEmpAssetsValidator pkh () () ctx =
--     traceIfFalse "beneficiary's signature missing" signedByBeneficiary
--     where
--         info :: TxInfo
--         info = scriptContextTxInfo ctx

--         signedByBeneficiary :: Bool
--         signedByBeneficiary = txSignedBy info pkh

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
    where file = printf "assets/%s-assets.plutus" name
