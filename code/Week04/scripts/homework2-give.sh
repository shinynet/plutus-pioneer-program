#!/bin/bash

assets=/workspace/code/Week02/assets
keypath=/workspace/keys
name="$1"
txin="$2"
body="$assets/homework2-give.txbody"
tx="$assets/homework2-give.tx"

# Build homework1 address 
cardano-cli address build \
    --payment-script-file "$assets/homework2.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/homework2.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in "120d62fe87a8434827231e777cf905583308cfee81f38449c56575c51582ade2#0" \
    --tx-in "5c95ee88a3f5a2558b5327a3bb41fd5cf0189af2e03008a0fcf92249f71fd56f#0" \
    --tx-out "$(cat "$assets/homework2.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$assets/unit.json" \
    --change-address "$(cat "$keypath/$name.addr")" \
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