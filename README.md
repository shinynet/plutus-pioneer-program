<h1 align="center">
  <br>
  Plutus Asset Manager
  <br>
</h1>

The Plutus Asset Manager is a prototype for tracking assets. A typical use case is
a business or corporation that lends out devices (laptops, tablets, etc) to
employees. These devices are represented by native assets and the blockchain is
used to track possession of these assets.

## Scenario 1: A new employee (Bob) joins the company and needs a company laptop. The company orders the laptop and once it arrives, a token representing that laptop needs minted.
---

### We first need to query available UTXOs from they asset dept wallet (keys already provided in the `/keys` dir). Open a terminal window and execute the the following utility and locate a UTXO with more than 10 ADA to facilitate the minting process.

```
scripts/query-address.sh $(cat keys/dept.addr)
```

### Open another terminal window and cd into `code/AssetManager/` Execute the following minting script replacing the TxHash & Index from your chosen UTXO

```
root@87a88ebe008f:/workspace/code/AssetManager# scripts/mint.sh \
> dept \
> "12345-678" \
> 79224d4411ff63a69100aabd258974fbf8ac7f14e8f56d35282256fa18eb307d#1
```

### The arguments are as follows:
1. The wallet owner creating the minting transaction.
2. The asset name (device name, serial no, etc).
3. The spending UTXO.

### After waiting a minute or so, re-query the Asset Dept's address until you see the new UTXO containing the newly minted asset. If there's multiple assets in the wallet, look for the one that includes 10 ADA.

## Scenario 2: The laptop has been given to Bob. The token should be transferred to a contract associated with Bob to represent Bob's possession of the laptop.
---

### In the terminal window where you executed the minting script, execute another script to transfer the asset to Bob (again replacing UTXOs as needed)

```
root@87a88ebe008f:/workspace/code/AssetManager# scripts/give.sh \
> dept \
> bob \
> "12345-678" \
> 603b497928a424bb40318ea1c26e927cf8c451bd23b7cd49fa9e36a36bce0479#0  
```

### The arguments are as follows:
1. The wallet owner creating the transfer transaction.
2. The wallet name of the employee the asset is being transfered to.
3. The name of the asset being transferred.
4. The UTXO hash/index pair containing the asset being transferred.

### After waiting a short while, you can re-query the Asset Dept's address to see the token removed, and query the address of Bob's smart contract to see the asset added

```
root@87a88ebe008f:/workspace# scripts/query-address.sh $(cat keys/dept.addr)

root@87a88ebe008f:/workspace# scripts/query-address.sh $(cat code/AssetManager/assets/bob-script.addr ) 
```

## Scenario 3: Bob resigns and needs to return the laptop to the Asset Dept.
---

### Execute a final script to unlock the asset from the contract and return it to the Asset Dept's wallet (again, replacing UTXOs as needed)

```
root@87a88ebe008f:/workspace/code/AssetManager# scripts/grab.sh \
> dept \
> bob \
> "12345-678" \
> fdc4d4c13ac5998df6c0f3e25d4c1130085c44ea1d36f79cb46b1f6764cb0972#0 \
> becbc6084bc590d8fd5982b4fa61d7282e644d0b1af241c495165107cf08223a#1
```

### The arguments are as follows:
1. The wallet owner creating the unlock/transfer transaction.
2. The wallet name of the employee the associated with the contract.
3. The name of the asset being transferred.
4. The UTXO hash/index pair in the contract containing the asset being transferred.
5. Any (ada-only) UTXO from the asset dept to use as collateral.
