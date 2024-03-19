module Noble.Secp256k1.Schnorr
  ( module X
  , SchnorrPublicKey
  , SchnorrSignature(SchnorrSignature)
  , signSchnorr
  , verifySchnorr
  , getSchnorrPublicKey
  , mkSchnorrPublicKey
  , unSchnorrPublicKey
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.ByteArray (ByteArray, byteLength)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (Message, PrivateKey)
import Noble.Secp256k1.ECDSA (Message, PrivateKey) as X

--------------------------------------------------------------------------------
-- SchnorrPublicKey
--------------------------------------------------------------------------------

newtype SchnorrPublicKey = SchnorrPublicKey ByteArray

mkSchnorrPublicKey :: ByteArray -> Maybe SchnorrPublicKey
mkSchnorrPublicKey uint8array
  | byteLength uint8array == 32 = Just $ SchnorrPublicKey uint8array
  | otherwise = Nothing

unSchnorrPublicKey :: SchnorrPublicKey -> ByteArray
unSchnorrPublicKey (SchnorrPublicKey uint8array) = uint8array

instance Show SchnorrPublicKey where
  show (SchnorrPublicKey x) = "(SchnorrPublicKey " <> show x <> ")"

derive newtype instance Eq SchnorrPublicKey
derive newtype instance Ord SchnorrPublicKey

--------------------------------------------------------------------------------
-- SchnorrSignature
--------------------------------------------------------------------------------

newtype SchnorrSignature = SchnorrSignature ByteArray

derive instance Generic SchnorrSignature _

instance Show SchnorrSignature where
  show (SchnorrSignature x) = "(SchnorrSignature " <> show x <> ")"

derive newtype instance Eq SchnorrSignature
derive newtype instance Ord SchnorrSignature

signSchnorr :: Message -> PrivateKey -> Aff SchnorrSignature
signSchnorr message privateKey = toAffE $ _sign message privateKey

verifySchnorr :: SchnorrSignature -> Message -> SchnorrPublicKey -> Aff Boolean
verifySchnorr signature message publicKey = toAffE $ _verify signature message
  publicKey

foreign import getSchnorrPublicKey :: PrivateKey -> SchnorrPublicKey

foreign import _sign
  :: Message -> PrivateKey -> Effect (Promise SchnorrSignature)

foreign import _verify
  :: SchnorrSignature -> Message -> SchnorrPublicKey -> Effect (Promise Boolean)
