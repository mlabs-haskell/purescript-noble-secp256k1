module Noble.Secp256k1.Schnorr
  ( module X
  , SchnorrPublicKey
  , SchnorrSignature
  , signSchnorr
  , verifySchnorr
  , getSchnorrPublicKey
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Function (on)
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (Message, PrivateKey)
import Noble.Secp256k1.ECDSA (Message, PrivateKey) as X


newtype SchnorrPublicKey = SchnorrPublicKey Uint8Array

newtype SchnorrSignature = SchnorrSignature Uint8Array

signSchnorr :: Message -> PrivateKey -> Aff SchnorrSignature
signSchnorr message privateKey = toAffE $ _sign message privateKey

verifySchnorr :: SchnorrSignature -> Message -> SchnorrPublicKey -> Aff Boolean
verifySchnorr signature message publicKey = toAffE $ _verify signature message
  publicKey

foreign import _sign
  :: Message -> PrivateKey -> Effect (Promise SchnorrSignature)

foreign import _verify
  :: SchnorrSignature -> Message -> SchnorrPublicKey -> Effect (Promise Boolean)

foreign import getSchnorrPublicKey :: PrivateKey -> SchnorrPublicKey

instance Show SchnorrPublicKey where
  show x = "(SchnorrPublicKey " <> _showBytes x <> ")"

instance Eq SchnorrPublicKey where
  eq = eqViaShow

instance Ord SchnorrPublicKey where
  compare = compareViaShow

instance Show SchnorrSignature where
  show x = "(SchnorrSignature " <> _showBytes x <> ")"

instance Eq SchnorrSignature where
  eq = eqViaShow

instance Ord SchnorrSignature where
  compare = compareViaShow

foreign import _showBytes :: forall a. a -> String

compareViaShow :: forall a. a -> a -> Ordering
compareViaShow = compare `on` _showBytes

eqViaShow :: forall a. a -> a -> Boolean
eqViaShow = eq `on` _showBytes
