{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server
  ( startServer
  , app
  , State(..)
  ) where

import Control.Concurrent.STM.TVar
       (TVar, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson
import Data.ByteString.Lazy.Internal (packChars)
import Data.List (find, intercalate)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant

import Rules
import TransactionLib

type BroadcastTransaction
   = "broadcast" :> ReqBody '[ JSON] SimpleTransaction :> Post '[ JSON] Text

type GetTransaction
   = "get" :> ReqBody '[ JSON] TransactionId :> Post '[ JSON] Transaction

type AllTransactions = "all" :> Get '[ JSON] [Transaction]

type TrialChainAPI
   = BroadcastTransaction :<|> GetTransaction :<|> AllTransactions

api :: Proxy TrialChainAPI
api = Proxy

newtype State = State
  { transactions :: TVar [Transaction]
  }

type AppM = ReaderT State Handler

server :: ServerT TrialChainAPI AppM
server = broadcastTransaction :<|> getTransaction :<|> allTransactions

-- | Rules that will be applied when trying to broadcast a transaction
activeRules :: [Rule]
activeRules =
  [amountIsPositive, transactionAlreadyExists, differentUsers, signatureIsValid] --, senderHasEnoughMoney]

-- | Broadcast new transaction to the netowrk
-- TODO: Should broadcast calling urls with the new transaction
broadcastTransaction :: SimpleTransaction -> AppM Text
broadcastTransaction t = do
  State {transactions = txs} <- ask
  transactionList <- liftIO $ readTVarIO txs
  let newTransaction = createTransaction t
      ruleErrors = checkRules newTransaction transactionList activeRules
  if not (null ruleErrors)
    then throwError err400 {errBody = packChars (intercalate " " ruleErrors)}
    else do
      liftIO $ atomically $ readTVar txs >>= writeTVar txs . (newTransaction :)
      pure $ txid (newTransaction :: Transaction)

-- | Get a transaction from server
getTransaction :: TransactionId -> AppM Transaction
getTransaction TransactionId {txid = txIdentifier} = do
  State {transactions = txs} <- ask
  transactionList <- liftIO $ readTVarIO txs
  case matchTransaction txIdentifier transactionList of
    Nothing -> throwError err404 {errBody = encode "Transaction not found."}
    Just t -> pure t

-- | List transactions broadcasted so far
allTransactions :: AppM [Transaction]
allTransactions = do
  State {transactions = txs} <- ask
  liftIO $ readTVarIO txs

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

startServer :: IO ()
startServer = do
  let port = 8080
  initialTransactions <- atomically $ newTVar []
  withStdoutLogger $ \aplogger -> do
    let settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings $ app $ State initialTransactions
