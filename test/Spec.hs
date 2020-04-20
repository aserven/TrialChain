{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Server (State(..), app)

import Control.Concurrent.STM.TVar (newTVar)
import Control.Monad.STM (atomically)
import Data.Text (Text)
import Network.HTTP.Types (methodPost)

import Test.Hspec
import Test.Hspec.Wai hiding (post)
import Test.Hspec.Wai.JSON

main :: IO ()
main = do
  initialTransactions <- atomically $ newTVar []
  let s = State initialTransactions
  hspec $ apiSpec s

post path = request methodPost path headers
  where
    headers = [("Content-Type", "application/json")]

apiSpec :: State -> Spec
apiSpec s = do
  let from' = "JQAg5ngbNf/5PR5PcS7EJ+4Wqc+WbefMhpq0vvz8hvw=" :: Text
      to' = "PCDdhp670xl/kylkvvxylOeyioucOmQX4YFVm+bZfrs=" :: Text
      hash =
        "09a90a6f5cd03444f98b4071430699c37259675af3a51a3e5281f3368dbb70aa" :: Text
      signature1 =
        "EtRpsXuARtx2QVY3b97XV9tQpnauus4bdgNXd7MkmSWCcDm7JRys8te9wtwjvFTubGtpazPmzMrJ69HbRmhqBg==" :: Text
      signature2 =
        "usixcifFiB9AO1Spke9MzSkeEUXPN6C1w3SIICklQWygFp0sE+1VhwkY2zb1r8OQPdqNoHBl26VFV/gnh6HXAw==" :: Text
      signature3 =
        "KsvcqI8318ZL/+6hffe1aFsp3sFezW5+LQdR8bhWEucWGfaYKqoFi9/C/ZjpjNjCuaVvWF8qQh0SVPmdoaw5Bg==" :: Text
  with (return (app s)) $ do
    describe "/broadcast" $ do
      it "responds with 400: Signature not valid" $
        post
          "/broadcast"
          [json|{from:#{from'}, to:#{to'},amount:56, timestamp: 123456789, signature: #{signature2}}|] `shouldRespondWith`
        "Signature is not valid." {matchStatus = 400}
      it "responds with 400: From == To" $
        post
          "/broadcast"
          [json|{from:#{from'}, to:#{from'},amount:56, timestamp: 123456789, signature: #{signature2}}|] `shouldRespondWith`
        "From and To must be different users." {matchStatus = 400}
      it "responds with 400: Amount must be positive" $
        post
          "/broadcast"
          [json|{from:#{from'}, to:#{to'},amount:-56, timestamp: 123456789, signature: #{signature3}}|] `shouldRespondWith`
        "Amount must be greater than 0." {matchStatus = 400}
      it "creates a valid transaction" $
        post
          "/broadcast"
          [json|{from:#{from'}, to:#{to'},amount:56, timestamp: 123456789, signature: #{signature1}}|] `shouldRespondWith`
        [json|#{hash}|]
      it "responds with 400: Transaction already exists" $
        post
          "/broadcast"
          [json|{from:#{from'}, to:#{to'},amount:56, timestamp: 123456789, signature: #{signature1}}|] `shouldRespondWith`
        "Transaction already exists." {matchStatus = 400}
    describe "/get" $ do
      it "transaction does not exist" $
        post "/get" [json|{txid: "nonexistentxid"}|] `shouldRespondWith` 404
      it "returns an existing transaction" $
        post "/get" [json|{txid: #{hash}}|] `shouldRespondWith`
        [json|{from:#{from'}, to:#{to'},amount:56, timestamp: 123456789, signature: #{signature1}, txid: #{hash}}|]
      it "responds with 400: Invalid data" $
        post "/get" [json|{nonvalid: ""}|] `shouldRespondWith` 400
