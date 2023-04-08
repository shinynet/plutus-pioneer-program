#!/bin/bash

name="$1"
emp="$2"
tokenname=$(echo -n "$3" | xxd -ps | tr -d '\n')
txin="$4"
collateral="$5"
assets=/workspace/code/AssetManager/assets
keypath=/workspace/keys
body="$assets/grab-$emp-script.txbody"
tx="$assets/grab-$emp-script.tx"
witness="$assets/$emp.witness"

# Calculate the PolicyId
policyid=$(
    cardano-cli transaction policyid \
        --script-file "$assets/mint-asset.plutus"
)

# Build the transaction
cardano-cli transaction build  \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/$emp-script.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/unit.json" \
    --tx-in-collateral "$collateral" \
    --tx-out $(cat $keypath/$name.addr)+2000000+"1 $policyid.$tokenname" \
    --change-address $(cat "$keypath/$name.addr") \
    --required-signer "$keypath/$name.skey" \
    --required-signer "$keypath/$emp.skey" \
    --protocol-params-file "$assets/protocol-parameters.json" \
    --out-file "$body"

# TODO: Have Dept and Emp sign transaction 
# separately using witness files

# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --signing-key-file "$keypath/$emp.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"