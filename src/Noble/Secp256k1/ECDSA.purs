module Noble.Secp256k1.ECDSA
  ( signECDSA
  , signECDSAWithRecoveredBit
  , getECDSAPublicKey
  , getECDSASharedSecret
  , recoverECDSAPublicKey
  , mkPrivateKey
  , mkECDSAPublicKey
  , mkMessageHash
  , unPrivateKey
  , unECDSAPublicKey
  , unMessageHash
  , unECDSASignature
  , verifyECDSA
  , DER
  , ECDSAPublicKey
  , Message
  , MessageHash
  , ECDSASignature(ECDSASignature)
  , ECDSASharedSecret(ECDSASharedSecret)
  , ECDSARecoveredBit
  , IsCompressed
  , PrivateKey
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.ByteArray (ByteArray(ByteArray), byteLength)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
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

type Message = ByteArray

type IsCompressed = Boolean

type DER = Boolean

type ECDSARecoveredBit = Number

--------------------------------------------------------------------------------
-- PrivateKey
--------------------------------------------------------------------------------

newtype PrivateKey = PrivateKey ByteArray

mkPrivateKey :: ByteArray -> Maybe PrivateKey
mkPrivateKey uint8arr
  | _isValidPrivateKey uint8arr = Just $ PrivateKey uint8arr
  | otherwise = Nothing

unPrivateKey :: PrivateKey -> ByteArray
unPrivateKey (PrivateKey uint8arr) = uint8arr

instance Show PrivateKey where
  show _ = "<PrivateKey contents not exposed>"

derive newtype instance Eq PrivateKey
derive newtype instance Ord PrivateKey

--------------------------------------------------------------------------------
-- PublicKey
--------------------------------------------------------------------------------

newtype ECDSAPublicKey = ECDSAPublicKey ByteArray

mkECDSAPublicKey :: ByteArray -> Maybe ECDSAPublicKey
mkECDSAPublicKey uint8arr
  | byteLength uint8arr == 32 = Just $ ECDSAPublicKey uint8arr
  | otherwise = Nothing

unECDSAPublicKey :: ECDSAPublicKey -> ByteArray
unECDSAPublicKey (ECDSAPublicKey uint8arr) = uint8arr

instance Show ECDSAPublicKey where
  show (ECDSAPublicKey x) = "(ECDSAPublicKey " <> show x <> ")"

derive newtype instance Eq ECDSAPublicKey
derive newtype instance Ord ECDSAPublicKey

--------------------------------------------------------------------------------
-- MessageHash
--------------------------------------------------------------------------------

newtype MessageHash = MessageHash ByteArray

mkMessageHash :: ByteArray -> Maybe MessageHash
mkMessageHash uint8arr
  | byteLength uint8arr == 32 = Just $ MessageHash uint8arr
  | otherwise = Nothing

unMessageHash :: MessageHash -> ByteArray
unMessageHash (MessageHash uint8arr) = uint8arr

instance Show MessageHash where
  show (MessageHash x) = "(MessageHash " <> show x <> ")"

derive newtype instance Eq MessageHash
derive newtype instance Ord MessageHash

--------------------------------------------------------------------------------
-- ECDSASignature
--------------------------------------------------------------------------------

newtype ECDSASignature = ECDSASignature ByteArray

unECDSASignature :: ECDSASignature -> ByteArray
unECDSASignature (ECDSASignature s) = s

derive instance Newtype ECDSASignature _

instance Show ECDSASignature where
  show (ECDSASignature x) = "(ECDSASignature " <> show x <> ")"

derive newtype instance Eq ECDSASignature
derive newtype instance Ord ECDSASignature

--------------------------------------------------------------------------------
-- ECDSASharedSecret
--------------------------------------------------------------------------------

newtype ECDSASharedSecret = ECDSASharedSecret ByteArray

derive instance Newtype ECDSASharedSecret _

instance Show ECDSASharedSecret where
  show (ECDSASharedSecret x) = "(ECDSASharedSecret " <> show x <> ")"

derive newtype instance Eq ECDSASharedSecret
derive newtype instance Ord ECDSASharedSecret

foreign import getECDSAPublicKey :: PrivateKey -> IsCompressed -> ECDSAPublicKey

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

foreign import _sign
  :: MessageHash -> PrivateKey -> DER -> Effect (Promise ECDSASignature)

foreign import _signWithRecoveredBit
  :: (forall a b. a -> b -> Tuple a b)
  -> MessageHash
  -> PrivateKey
  -> DER
  -> Effect (Promise (Tuple ECDSASignature ECDSARecoveredBit))

foreign import _isValidPrivateKey :: ByteArray -> Boolean
