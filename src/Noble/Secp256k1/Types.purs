module Noble.Secp256k1.Types
  ( PrivateKey
  , PublicKey
  , Message
  , MessageHash
  , Signature
  , SharedSecret
  , RecoveredBit
  , IsCompressed
  ) where

import Prelude

import Data.Function (on)

type IsCompressed = Boolean

foreign import data PrivateKey :: Type
foreign import data PublicKey :: Type
foreign import data Message :: Type
foreign import data MessageHash :: Type
foreign import data Signature :: Type
foreign import data SharedSecret :: Type
foreign import data RecoveredBit :: Type

instance Show PrivateKey where
  show _ = "<PrivateKey contents not exposed>"

instance Eq PrivateKey where
  eq = eqViaShow

instance Ord PrivateKey where
  compare = compareViaShow

instance Show PublicKey where
  show x = "(PublicKey " <> _showBytes x <> ")"

instance Eq PublicKey where
  eq = eqViaShow

instance Ord PublicKey where
  compare = compareViaShow

foreign import _showBytes :: forall a. a -> String

compareViaShow :: forall a. a -> a -> Ordering
compareViaShow = compare `on` _showBytes

eqViaShow :: forall a. a -> a -> Boolean
eqViaShow = eq `on` _showBytes
