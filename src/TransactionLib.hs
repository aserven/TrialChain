{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TransactionLib
  ( SimpleTransaction(..)
  , Transaction(..)
  , TransactionId(..)
  , createTransaction
  , getTransactionsMoney
  , existsTransaction
  , matchTransaction
  , hashTransaction
  ) where

import Crypto.Hash (SHA256(..), hashWith)
import Data.Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.List (find)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

data TransactionType
  = Payment
  | CoinCreation

data SimpleTransaction = SimpleTransaction
  { from :: T.Text
  , to :: T.Text
  , amount :: Int
  , signature :: T.Text
  , timestamp :: Integer
  }

data Transaction = Transaction
  { from :: T.Text
  , to :: T.Text
  , amount :: Int
  , signature :: T.Text
  , timestamp :: Integer
  , txid :: T.Text
  }

newtype TransactionId = TransactionId
  { txid :: T.Text
  }

$(deriveJSON defaultOptions ''SimpleTransaction)

$(deriveJSON defaultOptions ''TransactionId)

$(deriveJSON defaultOptions ''Transaction)

-- | Hash a transaction with SHA256, returns it in Base16
hashTransaction :: T.Text -> T.Text
hashTransaction txid = decodeUtf8 digest
  where
    bs = encodeUtf8 txid
    digest = convertToBase Base16 (hashWith SHA256 bs)

-- | Creates a Transaction from a SimpleTransaction
createTransaction :: SimpleTransaction -> Transaction
createTransaction SimpleTransaction { from = from'
                                    , to = to'
                                    , amount = a
                                    , timestamp = ts
                                    , signature = s
                                    } = newTransaction
  where
    txid =
      hashTransaction $ T.concat [to', (T.pack . show) a, (T.pack . show) ts]
    newTransaction =
      Transaction
      { from = from'
      , to = to'
      , amount = a
      , signature = s
      , timestamp = ts
      , txid = txid
      }

-- | Returns the amount that the user has
getTransactionsMoney :: T.Text -> [Transaction] -> Int
getTransactionsMoney user txs = foldl (\x y -> x + y) 0 money
  where
    money = fmap (\t -> amount (t :: Transaction)) transactions
    transactions = filter sameSender txs
    sameSender :: Transaction -> Bool
    sameSender Transaction {to = to'} = to' == user

-- | Checks if a transaction is already in the list
existsTransaction :: T.Text -> [Transaction] -> Bool
existsTransaction _ [] = False
existsTransaction txIdentifier (t:ts)
  | txIdentifier == txid (t :: Transaction) = True
  | otherwise = existsTransaction txIdentifier ts

-- | Returns  the transaction with id=txIdentifier if exists
matchTransaction :: T.Text -> [Transaction] -> Maybe Transaction
matchTransaction txIdentifier = find sameId
  where
    sameId :: Transaction -> Bool
    sameId Transaction {txid = txid'} = txIdentifier == txid'
