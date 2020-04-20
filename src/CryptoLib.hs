module CryptoLib
  ( createKeyPair
  , sign
  , verify
  ) where

import qualified Crypto.Sign.Ed25519 as ED
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64 as B64

type PublicKey = S.ByteString

type PrivateKey = S.ByteString

type Message = S.ByteString

type Signature = S.ByteString

-- | Wrapper around ed25519 createKeyPair to return Base64 encoded ByteStrings
createKeyPair :: IO (PublicKey, PrivateKey)
createKeyPair = do
  (pk, sk) <- ED.createKeypair
  let pk64 = B64.encode $ ED.unPublicKey pk
      sk64 = B64.encode $ ED.unSecretKey sk
  pure (pk64, sk64)

-- | Wrapper around ed25519 dsign to return Base64 encoded ByteStrings
sign :: PrivateKey -> Message -> Either String Signature
sign sk message = do
  decodedSecret <- B64.decode sk
  let secretKey = ED.SecretKey {ED.unSecretKey = decodedSecret}
      signature = ED.dsign secretKey message
      signedMessage = B64.encode $ ED.unSignature signature
  pure signedMessage

-- | Wrapper around ed25519 dverify to return Base64 encoded ByteStrings
verify :: PublicKey -> Message -> Signature -> Either String Bool
verify pk message s = do
  decodedPublic <- B64.decode pk
  decodedSignature <- B64.decode s
  let publicKey = ED.PublicKey {ED.unPublicKey = decodedPublic}
      signature = ED.Signature {ED.unSignature = decodedSignature}
  pure $ ED.dverify publicKey message signature
