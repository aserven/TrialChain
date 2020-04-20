# TrialChain Blockchain API mock

This is a new blockchain made up from scratch. We have a server that pretends to be connected to the hypothetical TrialChain network, offering a rest API similar to that electrum servers.

## Transaction

Here we explain how a transaction of this TrialChain looks like. It is composed by this fields:

- **from**
- **to**
- **amount**
- **timestamp**
- **signature**
- **txid**


## APi Methods

### `broadcast`
Broadcasts TrialChain transactions to the TrialChain blockchain network.

### `get`
Retrieve a previously broadcasted transaction given its transaction id


## Concensus rules
The set of rules that TrialChain follows to whether a transaction is valid or not.

1. Amount must be greater than 0.
2. Sender and recevier cannot be the same user.
3. Transaction cannot exists already in the blockchain.
4. The transaction is signed by the sender and is correct.
5. The amount cannot exceed the money that the sender has.

All of this 5 rules are implemented to be evaluated, but the last one (5) is not being enforced. We should have a way to add the initial transactions without checking that rule (Proposal in #Posible improvements and further work).

# How to's

## Build project
```
stack build
```

## Run server
```
stack run server
```

## Test
```
stack test
```


# Possible improvements and further work

- Add a way to have initial transactions

