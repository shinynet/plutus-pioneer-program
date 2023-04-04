{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import Plutus.V2.Ledger.Api         (BuiltinData, POSIXTime, PubKeyHash,
                                     ScriptContext (scriptContextTxInfo), Validator,
                                     mkValidatorScript, TxInfo (txInfoValidRange))
import PlutusTx                     (applyCode, compile, liftCode)
import PlutusTx.Prelude             (Bool, (.), ($), (&&))
import Utilities                    (wrapValidator)
import Plutus.V2.Ledger.Contexts    (txSignedBy)
import Plutus.V1.Ledger.Interval    (contains, from)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = txSignedByBeneficiary && deadlinePassed
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        txSignedByBeneficiary :: Bool
        txSignedByBeneficiary = txSignedBy txInfo beneficiary

        deadlinePassed :: Bool
        deadlinePassed = contains (from deadline) $ txInfoValidRange txInfo

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
