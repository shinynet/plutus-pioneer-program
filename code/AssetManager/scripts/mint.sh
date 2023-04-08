#!/bin/bash

name="$1"
tokenname=$(echo -n "$2" | xxd -ps | tr -d '\n')
txin="$3"
assets=/workspace/code/AssetManager/assets
keypath=/workspace/keys
body="$assets/mint-asset.txbody"
tx="$assets/mint-asset.tx"

# Calculate the PolicyId
policyid=$(
    cardano-cli transaction policyid \
        --script-file "$assets/mint-asset.plutus"
)

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-collateral "$txin" \
    --tx-out $(cat $keypath/$name.addr)+10000000+"1 $policyid.$tokenname" \
    --mint "1 $policyid.$tokenname" \
    --minting-script-file "$assets/mint-asset.plutus" \
    --mint-redeemer-file "$assets/unit.json" \
    --change-address $(cat "$keypath/$name.addr") \
    --required-signer "$keypath/$name.skey" \
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