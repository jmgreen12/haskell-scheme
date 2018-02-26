module SchemeParserSpec (main, spec) where

import Test.Hspec
import SchemeParser (readExpr)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "SchemeParser.readExpr" $ do
        it "parses a string" $ do
            readExpr "\"abcd\"" `shouldBe` "Found value"
        
        it "parses a string with escaped quotes" $ do
            readExpr "\"abc\\\"d\"" `shouldBe` "Found value"

        it "parses a string with escaped characters" $ do
            readExpr "\"\\ttab\\nnewline\\rreturn\\\\slash\"" `shouldBe` "Found value"