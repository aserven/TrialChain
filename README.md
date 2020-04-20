# TrialChain Blockchain API mock

This is a new blockchain made up from scratch. We have a server that pretends to be connected to the hypothetical TrialChain network, offering a rest API similar to that electrum servers.

## Transaction

Here we explain how a transaction of this TrialChain looks like. It is composed by this fields:

- **from**: The public key of the sender.
- **to**: The public key of the receiver.
- **amount**: Amount to be sent (Must be an integer).
- **timestamp**: Timestamp, a way to add a unique identifier to the transaction, which is used to create the transaction id.
- **signature**: Signature using the private key of the sender, content should be the transaction identifier.
- **txid**: Transaction identifier. This is the resulting hash of the public key of the receiver (**to**) concatenated with the amount and the timestamp.

The transaction id contains the timestamp to be able to make a unique transaction, otherwise the receiver could broadcast as many transaction as they wanted if only the public key + the amount is signed. With the timestamp we make sure that the transaction can be unique.

## Server
This server assumes that all keys (**from**, **to**) have been generated using ED25519 curve and given in base64 encoding. Also the transaction identifier is hashed using SHA256 and the result is stored in hexadecimal.

This server has three methods available: `broadcast`, `get`, `all`.

Also to be able to track which transactions have been broadcasted we have added a list of transactions in the state of ther server. Simulating what could be the blockchain with its blocks. With that we can retrieve in the `get` endpoint a valid transaction previously broadcasted.

## API Methods

### [POST] `broadcast`
Broadcasts TrialChain transactions to the TrialChain blockchain network.

##### Body
`from`, `to`, `amount`, `timestamp` and `signature`.

*Example:*

```json
{
    "from": "JQAg5ngbNf/5PR5PcS7EJ+4Wqc+WbefMhpq0vvz8hvw=",
    "to": "QNYS+jim/z1ASPeKWHM3WboHUXcHc4TIyn1OLxAIE0Q=",
    "amount": 543,
    "timestamp": 1587412528,
    "signature": "TLU0l4wM23bhUT2BX/4zbpSTNPiiOOUe514VtDJZ/gGpqX7YmxsYAcvEkbVoZX5atk6T/QIordCo728L7MxwCQ=="
}
```

#### Returns

- *200* : The transaction has been successfully broadcasted. *Example*:
    ```json
    "a107a957840d188c1474c2ace265239f23d46353c4406e015f7f1f7caba0fc2b"
    ```
- *400* : Bad request with the reason.

### [POST] `get`
Retrieve a previously broadcasted transaction given its transaction id.

##### Body
`txid`

*Example:*
```json
{
    "txid": "a107a957840d188c1474c2ace265239f23d46353c4406e015f7f1f7caba0fc2b"
}
```

#### Returns

- *200* : The transaction requested with all its data.
- *404* : Transaction not found.
- *400* : Bad request with the reason.

### [GET] `all`
Return all the transactions the server has broadcasted so far.

## Concensus rules
The set of rules that TrialChain follows to whether a transaction is valid or not.

1. Amount must be greater than 0.
2. Sender and recevier cannot be the same user.
3. Transaction cannot exists already in the blockchain.
4. The transaction is signed by the sender and is correct.
5. The amount cannot exceed the money that the sender has.

All of this 5 rules are implemented to be evaluated, but the last one (5) is not being enforced. We should have a way to add the initial transactions without checking that rule (Proposal in [Posible improvements and further work]).

# How to's

### Build project
```
stack build
```

### Run server
```
stack run server
```

### Test
```
stack test
```

### Test manually
To test manually the API we added in this project a helper command line tool to create keys, sign messages, verify signatures and create transactions.

For any doubts you can ask for help (also works in any subcommand):
```
❯ stack run helper-cli -- --help
Helper cli

Usage: helper-cli (create-keys | sign-message | verify-signature | 
                    create-transaction | create-random-transaction)

Available options:
  -h,--help                Show this help text

Available commands:
  create-keys              Create key pair
  sign-message             Sign a message usign a private key
  verify-signature         Verify a signature usign a public key and the original message
  create-transaction       Create a transaction given from, to and amount
  create-random-transaction
                           Create a random transaction
```

##### Create new key pair
```
❯ stack run helper-cli create-keys
 - Public key: "0DNWQg71vDFUAxVSiDfKqQ0nP+0oNgxnBDkYjLDZmz4="
 - Private key: "ahUPTZjRjjJYdGnvXKi0yKiX4RxUjbj2NsnOuw+90r7QM1ZCDvW8MVQDFVKIN8qpDSc/7Sg2DGcEORiMsNmbPg=="
Created in "public.key" and "private.key"
```

##### Sign a message
We have to pass the public key with the amount and timestamp (Ej. `date +%s`) and this helper will hash that for us.
```
❯ stack run helper-cli sign-message private.key akglomHZO9bLPM/YgK9Sy30xf0K1/DnBQSH1Rk/djxY=551587423066
 - You entered the message: "akglomHZO9bLPM/YgK9Sy30xf0K1/DnBQSH1Rk/djxY=551587423066"
 - SHA256 hash: "dd0e50a0614058d33e6f6d5662f33cf1ccf5480b2bd5c7ef729ddc5f6cbf7f72"
 - Signature: "hhjPTcjE0UatzTTtEyAGszB5yGHYoIE5MbEEeNLDisKThPfMdgZ6XLGfqgDYsM2JkWc7xS1ylOw1SOdIhn3fAQ=="
Created signature in "signature.txt" and hash in "message.txt"
```

##### Verify signature
```
❯ stack run helper-cli verify-signature public.key message.txt signature.txt
Signature is True
```

##### Create a transaction
We have to provide the public and private keys from the sender, the public key of the receiver and the amount.
```
❯ stack run helper-cli create-transaction public.key private.key pub.key 55
Created transaction in "transaction.json"
```
I will store it in a `json` file:

```json
{
    "from": "0DNWQg71vDFUAxVSiDfKqQ0nP+0oNgxnBDkYjLDZmz4=",
    "to": "akglomHZO9bLPM/YgK9Sy30xf0K1/DnBQSH1Rk/djxY=",
    "amount": 55,
    "timestamp": 1587423742,
    "signature": "eaqHBVRMEDNyKxC09DQ9Mh3OJrZxfJG1EIF6BlAqiAyYOg/2wk10VioA56ye3CzDw4uF34+76fkaUkk6BT/0AQ=="
}
```

We can also generate random transactions with:
```
❯ stack run helper-cli create-random-transaction
Created transaction in "transaction2391.json"
```

#### Querying the API endpoints
With the transactions that we have generated previously we can test now the API endpoints. First we need the server running (`stack run server`) and then we can send request via command line for example (with `curl` or `httpie`).

*Example of broadcast:*
```bash
❯ curl -vX POST localhost:8080/broadcast --header "Content-Type: application/json" -d @transaction.json
❯ http POST :8080/broadcast @transaction2391.json

"9c22f6d1cb6ca57e8962fe414f4307d0ab830e545699abadb70d8538b35ed1e9"

```

*Example of get:*
```bash
❯ curl -vX POST localhost:8080/get --header "Content-Type: application/json" \
   --data '{"txid":"c6b96e6d02b252f4cffd27a73466c9fc5f2d79bf1ce094dda64eac6b333f78b5"}'
❯ http :8080/get txid=9c22f6d1cb6ca57e8962fe414f4307d0ab830e545699abadb70d8538b35ed1e9

{
    "amount": 2391,
    "from": "O8R65pafP0/WkJ76sAJLHjYe70/Ycx7A8PcID1RwIzQ=",
    "signature": "iedeZJ6dYT7AyuzCA5k3LG36unFmDkummjz8hDYOLmOnubKCCYzmnmioXguJeZ0n9iZS1+uk3XigCYwOnsCfBg==",                                                                                                  
    "timestamp": 1587423793,
    "to": "7I3iOTU7KczpRJCfSe054iqeugvpKoVcu05dO4e9ZJ8=",
    "txid": "9c22f6d1cb6ca57e8962fe414f4307d0ab830e545699abadb70d8538b35ed1e9"
}
```

*Example of all:*
```bash
❯ curl localhost:8080/all
❯ http :8080/all

[
    ... All transactions 
]

```



## Possible improvements and further work

- Apply the rule that checks if the sender has enough money. We can do it with:
    - Add special transactions that can bypass this rule so we can populate the blockchain with initial transactions.
    - Add manually some initial transactions before starting the server.
- Use a database to store transactions.
- Implement blockchain blocks and store there the transactions.
- Add property checking tests and improve coverage.
- Setup CI/CD pipeline.
- Dockerize the application.

