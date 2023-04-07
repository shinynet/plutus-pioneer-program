#!/bin/bash

name="$1"
emp="$2"
tokenname=$(echo -n "$3" | xxd -ps | tr -d '\n')
txin="$4"
assets=/workspace/code/AssetManager/assets
keypath=/workspace/keys
body="$assets/xfer-asset-$emp.txbody"
tx="$assets/xfer-asset-$emp.tx"

# Build the transaction
cardano-cli transaction build  \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/homework1.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/homework1-redeemer-success.json" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --protocol-params-file "$pp" \
    --out-file "$body"




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