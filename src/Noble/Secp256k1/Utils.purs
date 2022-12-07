module Noble.Secp256k1.Utils
  ( sha256
  , randomPrivateKey
  , hashToPrivateKey
  , isValidPrivateKey
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (Message, MessageHash, PrivateKey)

sha256 :: Message -> Aff MessageHash
sha256 bytes = toAffE $ _sha256 bytes

hashToPrivateKey :: Uint8Array -> Maybe PrivateKey
hashToPrivateKey = _hashToPrivateKey Nothing Just

foreign import randomPrivateKey :: Effect PrivateKey

foreign import _sha256 :: Message -> Effect (Promise MessageHash)

foreign import _hashToPrivateKey
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Uint8Array
  -> Maybe PrivateKey

foreign import isValidPrivateKey :: PrivateKey -> Boolean
