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
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Function (on)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (Message, PrivateKey)
import Noble.Secp256k1.ECDSA (Message, PrivateKey) as X
import Noble.Secp256k1.Internal.Helpers (byteLength, compareIntArray, showBytes)

--------------------------------------------------------------------------------
-- SchnorrPublicKey
--------------------------------------------------------------------------------

newtype SchnorrPublicKey = SchnorrPublicKey Uint8Array

mkSchnorrPublicKey :: Uint8Array -> Maybe SchnorrPublicKey
mkSchnorrPublicKey uint8array
  | byteLength uint8array == 32 = Just $ SchnorrPublicKey uint8array
  | otherwise = Nothing

unSchnorrPublicKey :: SchnorrPublicKey -> Uint8Array
unSchnorrPublicKey (SchnorrPublicKey uint8array) = uint8array

instance Show SchnorrPublicKey where
  show (SchnorrPublicKey x) = "(SchnorrPublicKey " <> showBytes x <> ")"

instance Eq SchnorrPublicKey where
  eq x y = (compareIntArray `on` unSchnorrPublicKey) x y == EQ

instance Ord SchnorrPublicKey where
  compare = compareIntArray `on` unSchnorrPublicKey

--------------------------------------------------------------------------------
-- SchnorrSignature
--------------------------------------------------------------------------------

newtype SchnorrSignature = SchnorrSignature Uint8Array

derive instance Newtype SchnorrSignature _

instance Show SchnorrSignature where
  show (SchnorrSignature x) = "(SchnorrSignature " <> showBytes x <> ")"

instance Eq SchnorrSignature where
  eq x y = (compareIntArray `on` unwrap) x y == EQ

instance Ord SchnorrSignature where
  compare = (compareIntArray `on` unwrap)

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
