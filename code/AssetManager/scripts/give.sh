#!/bin/bash

name="$1"
emp="$2"
tokenname=$(echo -n "$3" | xxd -ps | tr -d '\n')
txin="$4"
assets=/workspace/code/AssetManager/assets
keypath=/workspace/keys
body="$assets/give-$emp-script.txbody"
tx="$assets/give-$emp-script.tx"

# Calculate the PolicyId
policyid=$(
    cardano-cli transaction policyid \
        --script-file "$assets/mint-asset.plutus"
)

# Build emp script address
cardano-cli address build \
    --payment-script-file "$assets/$emp-script.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/$emp-script.addr"

# Build the transaction
cardano-cli transaction build  \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out $(cat $assets/$emp-script.addr)+5000000+"1 $policyid.$tokenname" \
    --tx-out-inline-datum-file "$assets/$name-pkh-datum.json" \
    --change-address $(cat "$keypath/$name.addr") \
    --protocol-params-file "$assets/protocol-parameters.json" \
    --out-file "$body"

# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"