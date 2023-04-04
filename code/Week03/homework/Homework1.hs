{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api       (BuiltinData, POSIXTime, PubKeyHash, 
                                             Validator, mkValidatorScript)
import           PlutusTx                   (compile, unstableMakeIsData)
import           PlutusTx.Prelude           (Bool (..), ($), (&&), (||))
import           Utilities                  (wrapValidator)
import           Plutus.V2.Ledger.Contexts  (ScriptContext(scriptContextTxInfo), 
                                             TxInfo(txInfoValidRange), txSignedBy)
import           Plutus.V1.Ledger.Interval  (contains, to, before)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator d () ctx =  (txSignedByBeneficiary1 && byDeadline)
                            || (txSignedByBeneficiary2 && afterDeadline)
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        txSignedByBeneficiary1 :: Bool
        txSignedByBeneficiary1 = txSignedBy txInfo $ beneficiary1 d

        txSignedByBeneficiary2 :: Bool
        txSignedByBeneficiary2 = txSignedBy txInfo $ beneficiary2 d

        byDeadline :: Bool
        byDeadline = contains (to $ deadline d) $ txInfoValidRange txInfo

        afterDeadline :: Bool
        afterDeadline = before (deadline d) $ txInfoValidRange txInfo


{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
