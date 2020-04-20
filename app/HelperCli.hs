{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mapM)
import qualified Data.ByteString as S
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Time.Clock.POSIX
import System.Directory (doesFileExist)
import System.Random (newStdGen, randomR)
import Turtle

import CryptoLib
import TransactionLib (hashTransaction)

main :: IO ()
main = cli

cli :: IO ()
cli = join (options "Helper cli" parser)

parser :: Parser (IO ())
parser =
  parseCreateKeys <|> parseSignMessage <|> parseVerifySignature <|>
  parseCreateTransaction <|>
  parseCreateRandomTransaction

parseCreateKeys :: Parser (IO ())
parseCreateKeys =
  fmap createKeys (subcommand "create-keys" "Create key pair" createKeysArgs)

createKeysArgs :: Parser (Maybe Text, Maybe Text)
createKeysArgs =
  (,) <$>
  optional
    (optText
       "output-public"
       'p'
       "Public key's output file. Default is 'public.key'") <*>
  optional
    (optText
       "output-private"
       's'
       "Private key's output file. Default is 'private.key'")

parseSignMessage :: Parser (IO ())
parseSignMessage =
  fmap
    signMessage
    (subcommand
       "sign-message"
       "Sign a message usign a private key"
       parseSignMessageArgs)

parseSignMessageArgs :: Parser (Text, Text, Maybe Text, Maybe Text)
parseSignMessageArgs =
  (,,,) <$> argText "key" "The private key to use (can be a file or string)" <*>
  argText
    "message"
    "The message or contents of file to sign. (NOTE: It will be hashed first)" <*>
  optional
    (optText
       "output-hash"
       'h'
       "Hash of the message output file. Default is 'message.txt") <*>
  optional
    (optText "output" 'o' "Signature output file. Default is 'signature.txt")

parseVerifySignature :: Parser (IO ())
parseVerifySignature =
  fmap
    verifySignature
    (subcommand
       "verify-signature"
       "Verify a signature usign a public key and the original message"
       parseVerifySignatureArgs)

parseVerifySignatureArgs :: Parser (Text, Text, Text)
parseVerifySignatureArgs =
  (,,) <$> argText "key" "The public key to use (can be a file or string)" <*>
  argText "message" "The original message or contents of file to verify" <*>
  argText "signature" "Signature to verify"

parseCreateTransaction :: Parser (IO ())
parseCreateTransaction =
  fmap
    createTransaction
    (subcommand
       "create-transaction"
       "Create a transaction given from, to and amount"
       parseCreateTransactionArgs)

parseCreateTransactionArgs :: Parser (Text, Text, Text, Int, Maybe Text)
parseCreateTransactionArgs =
  (,,,,) <$>
  argText "from" "The public key of the sender (can be a file or string)" <*>
  argText "private" "The private key to use to sign (can be a file or string)" <*>
  argText "to" "The public key of the receiver (can be a file or string)" <*>
  argInt "amount" "Amount to send (Int)" <*>
  optional
    (optText "output" 'o' "Transaction output file. Default is transaction.json")

parseCreateRandomTransaction :: Parser (IO ())
parseCreateRandomTransaction =
  subcommand
    "create-random-transaction"
    "Create a random transaction"
    (pure createRandomTransaction)

createKeys :: (Maybe Text, Maybe Text) -> IO ()
createKeys (op, os) = do
  (pk, sk) <- createKeyPair
  let outputPublic = getFromMaybe "public.key" op
      outputPrivate = getFromMaybe "private.key" os
  S.writeFile outputPublic pk
  S.writeFile outputPrivate sk
  putStrLn $ " - Public key: " ++ show pk
  putStrLn $ " - Private key: " ++ show sk
  putStrLn $ "Created in " ++ show outputPublic ++ " and " ++ show outputPrivate

getFromMaybe :: Text -> Maybe Text -> String
getFromMaybe d m = T.unpack $ maybe d id m

getFromFile :: Text -> IO S.ByteString
getFromFile t = do
  exists <- doesFileExist (T.unpack t)
  if exists
    then S.readFile (T.unpack t)
    else pure $ E.encodeUtf8 t

signMessage :: (Text, Text, Maybe Text, Maybe Text) -> IO ()
signMessage (key, msg, oh, os) = do
  sk <- getFromFile key
  message <- getFromFile msg
  let hash = E.encodeUtf8 $ hashTransaction (E.decodeUtf8 message)
      signature = sign sk hash
      outputHash = getFromMaybe "message.txt" oh
      outputSignature = getFromMaybe "signature.txt" os
  case signature of
    Left reason -> putStrLn $ "Something went wrong signing: " ++ show reason
    Right signature' -> do
      S.writeFile outputSignature signature'
      S.writeFile outputHash hash
      putStrLn $ " - You entered the message: " ++ show message
      putStrLn $ " - SHA256 hash: " ++ show hash
      putStrLn $ " - Signature: " ++ show signature'
      putStrLn $
        "Created signature in " ++
        show outputSignature ++ " and hash in " ++ show outputHash

verifySignature :: (Text, Text, Text) -> IO ()
verifySignature (key, msg, s) = do
  pk <- getFromFile key
  message <- getFromFile msg
  signature <- getFromFile s
  createRandomTransaction
  case verify pk message signature of
    Left reason -> putStrLn $ "Something went wrong verifying: " ++ show reason
    Right result -> putStrLn $ "Signature is " ++ show result

createTransaction :: (Text, Text, Text, Int, Maybe Text) -> IO ()
createTransaction (from, secret, to, a, out) = do
  fromPk <- getFromFile from
  fromSk <- getFromFile secret
  toPk <- getFromFile to
  ts <- getPOSIXTime
  let timestamp = floor ts
      amount = (fromString . show) a
      output = getFromMaybe "transaction.json" out
      h =
        hashTransaction
          (E.decodeUtf8 toPk <> amount <> (T.pack . show) timestamp)
      s = sign fromSk (E.encodeUtf8 h)
  case s of
    Left reason -> putStrLn $ "Something went wrong signing: " ++ show reason
    Right signature -> do
      S.writeFile output "{\n"
      mapM
        (S.appendFile output)
        [ ("    \"from\": \"" <> fromPk <> "\",\n")
        , ("    \"to\": \"" <> toPk <> "\",\n")
        , ("    \"amount\": " <> E.encodeUtf8 amount <> ",\n")
        , ("    \"timestamp\": " <> (fromString . show) timestamp <> ",\n")
        , ("    \"signature\": \"" <> signature <> "\"\n")
        , "}"
        ]
      putStrLn $ "Created transaction in " ++ output

createRandomTransaction :: IO ()
createRandomTransaction = do
  (fromPk, fromSk) <- createKeyPair
  (toPk, toSk) <- createKeyPair
  g <- newStdGen
  let (n, g1) = randomR (1 :: Int, 9999 :: Int) g
      out = T.pack ("transaction" ++ show n ++ ".json")
  createTransaction
    (E.decodeUtf8 fromPk, E.decodeUtf8 fromSk, E.decodeUtf8 toPk, n, Just out)
