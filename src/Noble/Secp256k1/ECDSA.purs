module Noble.Secp256k1.ECDSA
  ( signECDSA
  , signECDSAWithRecoveredBit
  , getECDSAPublicKey
  , getECDSASharedSecret
  , recoverECDSAPublicKey
  , verifyECDSA
  , DER
  , ECDSAPublicKey
  , ECDSARecoveredBit
  , ECDSASharedSecret
  , ECDSASignature
  , IsCompressed
  , Message
  , MessageHash
  , PrivateKey
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Function (on)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff)

signECDSA :: MessageHash -> PrivateKey -> DER -> Aff ECDSASignature
signECDSA msgHash privateKey der = toAffE $ _sign msgHash privateKey der

signECDSAWithRecoveredBit
  :: MessageHash
  -> PrivateKey
  -> DER
  -> Aff (Tuple ECDSASignature ECDSARecoveredBit)
signECDSAWithRecoveredBit msgHash privateKey der = toAffE $
  _signWithRecoveredBit
    Tuple
    msgHash
    privateKey
    der

foreign import getECDSAPublicKey :: PrivateKey -> IsCompressed -> ECDSAPublicKey

foreign import _sign
  :: MessageHash -> PrivateKey -> DER -> Effect (Promise ECDSASignature)

foreign import _signWithRecoveredBit
  :: (forall a b. a -> b -> Tuple a b)
  -> MessageHash
  -> PrivateKey
  -> DER
  -> Effect (Promise (Tuple ECDSASignature ECDSARecoveredBit))

foreign import verifyECDSA
  :: ECDSASignature
  -> MessageHash
  -> ECDSAPublicKey
  -> Boolean

foreign import getECDSASharedSecret
  :: PrivateKey
  -> ECDSAPublicKey
  -> IsCompressed
  -> ECDSASharedSecret

foreign import recoverECDSAPublicKey
  :: MessageHash
  -> ECDSASignature
  -> ECDSARecoveredBit
  -> IsCompressed
  -> ECDSAPublicKey

type IsCompressed = Boolean

foreign import data PrivateKey :: Type
foreign import data ECDSAPublicKey :: Type
foreign import data Message :: Type
foreign import data MessageHash :: Type
foreign import data ECDSASignature :: Type
foreign import data ECDSASharedSecret :: Type

type ECDSARecoveredBit = Number

type DER = Boolean

instance Show PrivateKey where
  show _ = "<PrivateKey contents not exposed>"

instance Eq PrivateKey where
  eq = eqViaShow

instance Ord PrivateKey where
  compare = compareViaShow

instance Show ECDSAPublicKey where
  show x = "(ECDSAPublicKey " <> _showBytes x <> ")"

instance Eq ECDSAPublicKey where
  eq = eqViaShow

instance Ord ECDSAPublicKey where
  compare = compareViaShow

instance Show Message where
  show x = "(Message " <> _showBytes x <> ")"

instance Eq Message where
  eq = eqViaShow

instance Ord Message where
  compare = compareViaShow

instance Show MessageHash where
  show x = "(MessageHash " <> _showBytes x <> ")"

instance Eq MessageHash where
  eq = eqViaShow

instance Ord MessageHash where
  compare = compareViaShow

instance Show ECDSASignature where
  show x = "(ECDSASignature " <> _showBytes x <> ")"

instance Eq ECDSASignature where
  eq = eqViaShow

instance Ord ECDSASignature where
  compare = compareViaShow

instance Show ECDSASharedSecret where
  show x = "(ECDSASharedSecret " <> _showBytes x <> ")"

instance Eq ECDSASharedSecret where
  eq = eqViaShow

instance Ord ECDSASharedSecret where
  compare = compareViaShow

foreign import _showBytes :: forall a. a -> String

compareViaShow :: forall a. a -> a -> Ordering
compareViaShow = compare `on` _showBytes

eqViaShow :: forall a. a -> a -> Boolean
eqViaShow = eq `on` _showBytes
