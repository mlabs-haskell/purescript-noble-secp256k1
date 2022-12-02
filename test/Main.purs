module Test.Main where

import Prelude

import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Noble.Secp256k1.ECDSA
  ( getPublicKey
  , getSharedSecret
  , recoverPublicKey
  , sign
  , signWithRecoveredBit
  , verify
  )
import Noble.Secp256k1.Types (Message)
import Noble.Secp256k1.Utils (randomPrivateKey, sha256)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Noble.Secp256k1.Utils" do
    it "randomPrivateKey" do
      _randomPk <- liftEffect $ randomPrivateKey
      pure unit
  describe "Noble.Secp256k1.ECDSA" do
    it "getPublicKey" do
      privateKey <- liftEffect $ randomPrivateKey
      let _publicKey = getPublicKey privateKey true
      pure unit
    it "sign/verify" do
      privateKey <- liftEffect $ randomPrivateKey
      let
        publicKey = getPublicKey privateKey true
        message = unsafeCoerce privateKey
      messageHash <- sha256 message
      signature <- sign messageHash privateKey
      let result = verify signature messageHash publicKey
      result `shouldEqual` true
      let wrongResult = verify (unsafeCoerce messageHash) messageHash publicKey
      wrongResult `shouldEqual` false
    it "signWithRecoveredBit/recoverPublicKey" do
      privateKey <- liftEffect $ randomPrivateKey
      let
        publicKey = getPublicKey privateKey true
        message = unsafeCoerce privateKey
      messageHash <- sha256 message
      Tuple signature recovered <- signWithRecoveredBit messageHash privateKey
      let result = verify signature messageHash publicKey
      result `shouldEqual` true
      let wrongResult = verify (unsafeCoerce messageHash) messageHash publicKey
      wrongResult `shouldEqual` false
      let
        recoveredPublicKey = recoverPublicKey messageHash signature recovered
          true
      recoveredPublicKey `shouldEqual` publicKey
    it "getSharedSecret" do
      privateKey1 <- liftEffect $ randomPrivateKey
      privateKey2 <- liftEffect $ randomPrivateKey
      let
        publicKey2 = getPublicKey privateKey2 true
        _sharedSecret = getSharedSecret privateKey1 publicKey2 false
      pure unit

foreign import data Bytes :: Type

foreign import randomBytes :: Int -> Bytes

messageFromBytes :: Bytes -> Message
messageFromBytes = unsafeCoerce
