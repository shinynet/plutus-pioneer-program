#!/bin/bash

name="$1"
emp="$2"
tokenname=$(echo -n "$3" | xxd -ps | tr -d '\n')
txin="$4"
assets=/workspace/code/AssetManager/assets
keypath=/workspace/keys
body="$assets/xfer-asset-$emp.txbody"
tx="$assets/xfer-asset-$emp.tx"

# Calculate the PolicyId
policyid=$(
    cardano-cli transaction policyid \
        --script-file "$assets/mint-asset.plutus"
)

# Build emp script address
cardano-cli address build \
    --payment-script-file "$assets/$emp-assets.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/$emp-assets.addr"

# Build the transaction
cardano-cli transaction build  \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in "dde3e3cc7933685d40ae0380f51210a6cf30f496281823a67eb4bf5164b832fd#1" \
    --tx-out $(cat $assets/$emp-assets.addr)+2000000+"1 $policyid.$tokenname" \
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