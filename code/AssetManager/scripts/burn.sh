#!/bin/bash


name="$1"
tokenname=$(echo -n "$2" | xxd -ps | tr -d '\n')
txin="$3"
collateral="$4"
assets=/workspace/code/Week05/assets
keypath=/workspace/keys
body="$assets/signed-mint.txbody"
tx="$assets/signed-mint.tx"
outaddress=$(cat $keypath/$name.addr)

# Build minting address 
cardano-cli address build \
    --payment-script-file "$assets/signed.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/homework1.addr"

# Calculate the PolicyId
policyid=$(
    cardano-cli transaction policyid \
        --script-file "$assets/signed.plutus"
)

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in 5867dbf3aa7875e0464cbe23881ec278591d661b7230d37e43be1f87d7190169#0 \
    --tx-in 5867dbf3aa7875e0464cbe23881ec278591d661b7230d37e43be1f87d7190169#1 \
    --tx-in-collateral "$collateral" \
    --tx-out $(cat $keypath/$name.addr)+2000000 \
    --mint "-1 $policyid.$tokenname" \
    --minting-script-file "$assets/signed.plutus" \
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