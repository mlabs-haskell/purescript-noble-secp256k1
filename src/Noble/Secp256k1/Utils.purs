module Noble.Secp256k1.Utils (sha256, randomPrivateKey) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.Types (Message, MessageHash, PrivateKey)

sha256 :: Message -> Aff MessageHash
sha256 bytes = toAffE $ _sha256 bytes

foreign import randomPrivateKey :: Effect PrivateKey

foreign import _sha256 :: Message -> Effect (Promise MessageHash)
