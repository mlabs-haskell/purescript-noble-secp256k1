module Test.Main where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(Just, Nothing), isJust, isNothing)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Noble.Secp256k1.ECDSA
  ( Message
  , getECDSAPublicKey
  , getECDSASharedSecret
  , recoverECDSAPublicKey
  , signECDSA
  , signECDSAWithRecoveredBit
  , verifyECDSA
  )
import Noble.Secp256k1.Schnorr (getSchnorrPublicKey, signSchnorr, verifySchnorr)
import Noble.Secp256k1.Utils
  ( hashToPrivateKey
  , isValidPrivateKey
  , randomPrivateKey
  , sha256
  )
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Noble.Secp256k1.Utils" do
    it "randomPrivateKey" do
      _randomPk <- liftEffect $ randomPrivateKey
      pure unit
    it "hashToPrivateKey" do
      hashToPrivateKey (randomBytes 40) `shouldSatisfy` isJust
      hashToPrivateKey (randomBytes 1024) `shouldSatisfy` isJust
      hashToPrivateKey (randomBytes 39) `shouldSatisfy` isNothing
      hashToPrivateKey (randomBytes 1025) `shouldSatisfy` isNothing
    it "isValidPrivateKey" do
      hashToPrivateKey (randomBytes 40) `shouldSatisfy` case _ of
        Nothing -> false
        Just key -> isValidPrivateKey key
      isValidPrivateKey (unsafeCoerce $ randomBytes 42) `shouldEqual` false
  describe "Noble.Secp256k1.ECDSA" do
    it "getECDSAPublicKey" do
      privateKey <- liftEffect $ randomPrivateKey
      let _publicKey = getECDSAPublicKey privateKey true
      pure unit
    it "signECDSA/verifyECDSA DER = false" do
      privateKey <- liftEffect $ randomPrivateKey
      let
        publicKey = getECDSAPublicKey privateKey true
        message = unsafeCoerce privateKey
      messageHash <- sha256 message
      signature <- signECDSA messageHash privateKey false
      let result = verifyECDSA signature messageHash publicKey
      result `shouldEqual` true
      let
        wrongResult = verifyECDSA (unsafeCoerce messageHash) messageHash
          publicKey
      wrongResult `shouldEqual` false
    it "signECDSA/verifyECDSA DER = true" do
      privateKey <- liftEffect $ randomPrivateKey
      let
        publicKey = getECDSAPublicKey privateKey true
        message = unsafeCoerce privateKey
      messageHash <- sha256 message
      signature <- signECDSA messageHash privateKey true
      let result = verifyECDSA signature messageHash publicKey
      result `shouldEqual` true
      let
        wrongResult = verifyECDSA (unsafeCoerce messageHash) messageHash
          publicKey
      wrongResult `shouldEqual` false
    it "signECDSAWithRecoveredBit/recoverECDSAPublicKey" do
      privateKey <- liftEffect $ randomPrivateKey
      let
        publicKey = getECDSAPublicKey privateKey true
        message = unsafeCoerce privateKey
      messageHash <- sha256 message
      Tuple signature recovered <- signECDSAWithRecoveredBit messageHash
        privateKey
        false
      let result = verifyECDSA signature messageHash publicKey
      result `shouldEqual` true
      let
        wrongResult = verifyECDSA (unsafeCoerce messageHash) messageHash
          publicKey
      wrongResult `shouldEqual` false
      let
        recoveredPublicKey = recoverECDSAPublicKey messageHash signature
          recovered
          true
      recoveredPublicKey `shouldEqual` publicKey
    it "getECDSASharedSecret" do
      privateKey1 <- liftEffect $ randomPrivateKey
      privateKey2 <- liftEffect $ randomPrivateKey
      let
        publicKey2 = getECDSAPublicKey privateKey2 true
        _sharedSecret = getECDSASharedSecret privateKey1 publicKey2 false
      pure unit
  describe "Noble.Secp256k1.Schnorr" do
    it "getSchnorrPublicKey / signSchnorr / verifySchnorr" do
      privateKey <- liftEffect $ randomPrivateKey
      let
        publicKey = getSchnorrPublicKey privateKey
        message = unsafeCoerce privateKey
      signature <- signSchnorr message privateKey
      result <- verifySchnorr signature message publicKey
      result `shouldEqual` true
      wrongResult <- verifySchnorr (unsafeCoerce privateKey) message publicKey
      wrongResult `shouldEqual` false

foreign import randomBytes :: Int -> Uint8Array

messageFromBytes :: Uint8Array -> Message
messageFromBytes = unsafeCoerce
