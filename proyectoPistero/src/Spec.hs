module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "funcion estaEnBuenEstado" $ do
    it "no esta en buen estado el Peugeot" $ do
      estaEnBuenEstado peugeot `shouldBe` False
