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
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Function (on)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Internal.Helpers (byteLength, compareIntArray, showBytes)

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

type Message = Uint8Array

type IsCompressed = Boolean

type DER = Boolean

type ECDSARecoveredBit = Number

--------------------------------------------------------------------------------
-- PrivateKey
--------------------------------------------------------------------------------

newtype PrivateKey = PrivateKey Uint8Array

mkPrivateKey :: Uint8Array -> Maybe PrivateKey
mkPrivateKey uint8arr
  | _isValidPrivateKey uint8arr = Just $ PrivateKey uint8arr
  | otherwise = Nothing

unPrivateKey :: PrivateKey -> Uint8Array
unPrivateKey (PrivateKey uint8arr) = uint8arr

instance Show PrivateKey where
  show _ = "<PrivateKey contents not exposed>"

instance Eq PrivateKey where
  eq x y = (compareIntArray `on` unPrivateKey) x y == EQ

instance Ord PrivateKey where
  compare = compareIntArray `on` unPrivateKey

--------------------------------------------------------------------------------
-- PublicKey
--------------------------------------------------------------------------------

newtype ECDSAPublicKey = ECDSAPublicKey Uint8Array

mkECDSAPublicKey :: Uint8Array -> Maybe ECDSAPublicKey
mkECDSAPublicKey uint8arr
  | byteLength uint8arr == 32 = Just $ ECDSAPublicKey uint8arr
  | otherwise = Nothing

unECDSAPublicKey :: ECDSAPublicKey -> Uint8Array
unECDSAPublicKey (ECDSAPublicKey uint8arr) = uint8arr

instance Show ECDSAPublicKey where
  show (ECDSAPublicKey x) = "(ECDSAPublicKey " <> showBytes x <> ")"

instance Eq ECDSAPublicKey where
  eq x y = (compareIntArray `on` unECDSAPublicKey) x y == EQ

instance Ord ECDSAPublicKey where
  compare = compareIntArray `on` unECDSAPublicKey

--------------------------------------------------------------------------------
-- MessageHash
--------------------------------------------------------------------------------

newtype MessageHash = MessageHash Uint8Array

mkMessageHash :: Uint8Array -> Maybe MessageHash
mkMessageHash uint8arr
  | byteLength uint8arr == 32 = Just $ MessageHash uint8arr
  | otherwise = Nothing

unMessageHash :: MessageHash -> Uint8Array
unMessageHash (MessageHash uint8arr) = uint8arr

instance Show MessageHash where
  show (MessageHash x) = "(MessageHash " <> showBytes x <> ")"

instance Eq MessageHash where
  eq x y = (compareIntArray `on` unMessageHash) x y == EQ

instance Ord MessageHash where
  compare = compareIntArray `on` unMessageHash

--------------------------------------------------------------------------------
-- ECDSASignature
--------------------------------------------------------------------------------

newtype ECDSASignature = ECDSASignature Uint8Array

derive instance Newtype ECDSASignature _

instance Show ECDSASignature where
  show (ECDSASignature x) = "(ECDSASignature " <> showBytes x <> ")"

instance Eq ECDSASignature where
  eq x y = (compareIntArray `on` unwrap) x y == EQ

instance Ord ECDSASignature where
  compare = compareIntArray `on` unwrap

--------------------------------------------------------------------------------
-- ECDSASharedSecret
--------------------------------------------------------------------------------

newtype ECDSASharedSecret = ECDSASharedSecret Uint8Array

derive instance Newtype ECDSASharedSecret _

instance Show ECDSASharedSecret where
  show (ECDSASharedSecret x) = "(ECDSASharedSecret " <> showBytes x <> ")"

instance Eq ECDSASharedSecret where
  eq x y = (compareIntArray `on` unwrap) x y == EQ

instance Ord ECDSASharedSecret where
  compare = compareIntArray `on` unwrap

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

foreign import _isValidPrivateKey :: Uint8Array -> Boolean
