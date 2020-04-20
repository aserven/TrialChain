module Rules
  ( Rule(..)
  , checkRules
  , amountIsPositive
  , transactionAlreadyExists
  , senderHasEnoughMoney
  , differentUsers
  , signatureIsValid
  ) where

import Data.Either (lefts)
import Data.Monoid ((<>))
import Data.Text (Text, concat, pack)
import Data.Text.Encoding (encodeUtf8)

import CryptoLib (verify)
import TransactionLib
       (Transaction(..), existsTransaction, getTransactionsMoney)

type Rule = Transaction -> [Transaction] -> Either String Bool

checkRules :: Transaction -> [Transaction] -> [Rule] -> [String]
checkRules t ts rs = lefts appliedRules
  where
    appliedRules = fmap (\r -> r t ts) rs

amountIsPositive :: Rule
amountIsPositive Transaction {amount = a} _
  | a < 0 = Left "Amount must be greater than 0."
  | otherwise = Right True

transactionAlreadyExists :: Rule
transactionAlreadyExists Transaction {txid = transactionId} ts =
  if existsTransaction transactionId ts
    then Left "Transaction already exists."
    else Right True

senderHasEnoughMoney :: Rule
senderHasEnoughMoney Transaction {to = sender, amount = a} ts =
  if getTransactionsMoney sender ts < a
    then Left "Sender does not have enough money."
    else Right True

differentUsers :: Rule
differentUsers Transaction {from = sender, to = receiver} _ =
  if sender == receiver
    then Left "From and To must be different users."
    else Right True

signatureIsValid :: Rule
signatureIsValid Transaction { from = sender
                             , amount = a
                             , timestamp = ts
                             , signature = s
                             , txid = txid'
                             } _ = do
  let msg = encodeUtf8 txid'
      from' = encodeUtf8 sender
      signature' = encodeUtf8 s
  verification <- verify from' msg signature'
  if verification == True
    then Right True
    else Left "Signature is not valid."
