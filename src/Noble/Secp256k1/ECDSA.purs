module Noble.Secp256k1.ECDSA
  ( sign
  , signWithRecoveredBit
  , getPublicKey
  , getSharedSecret
  , recoverPublicKey
  , verify
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.Types
  ( IsCompressed
  , MessageHash
  , PrivateKey
  , PublicKey
  , RecoveredBit
  , SharedSecret
  , Signature
  )

sign :: MessageHash -> PrivateKey -> Aff Signature
sign msgHash privateKey = toAffE $ _sign msgHash privateKey

signWithRecoveredBit
  :: MessageHash -> PrivateKey -> Aff (Tuple Signature RecoveredBit)
signWithRecoveredBit msgHash privateKey = toAffE $ _signWithRecoveredBit Tuple
  msgHash
  privateKey

foreign import getPublicKey :: PrivateKey -> IsCompressed -> PublicKey

foreign import _sign :: MessageHash -> PrivateKey -> Effect (Promise Signature)

foreign import _signWithRecoveredBit
  :: (forall a b. a -> b -> Tuple a b)
  -> MessageHash
  -> PrivateKey
  -> Effect (Promise (Tuple Signature RecoveredBit))

foreign import verify
  :: Signature
  -> MessageHash
  -> PublicKey
  -> Boolean

foreign import getSharedSecret
  :: PrivateKey
  -> PublicKey
  -> IsCompressed
  -> SharedSecret

foreign import recoverPublicKey
  :: MessageHash
  -> Signature
  -> RecoveredBit
  -> IsCompressed
  -> PublicKey
