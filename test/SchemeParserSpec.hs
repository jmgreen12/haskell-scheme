module SchemeParserSpec (main, spec) where

import LispError (extractValue)
import SchemeParser (readExpr)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "readExpr" $ do
        it "parses a string" $ do
            (show . extractValue . readExpr) "\"abcd\"" `shouldBe` "\"abcd\""
        
        it "parses a string with escaped quotes" $ do
            (show . extractValue . readExpr) "\"abc\\\"d\"" `shouldBe` "\"abc\"d\""

        it "parses a string with escaped characters" $ do
            (show . extractValue . readExpr) "\"\\ttab\\nnewline\\rreturn\\\\slash\"" `shouldBe` "\"\ttab\nnewline\rreturn\\slash\""
        
        it "parses a list" $ do
            (show . extractValue . readExpr) "(1 2 2)" `shouldBe` "(1 2 2)"

        it "parses a quoted literal" $ do
            (show . extractValue . readExpr) "'(1 3 (\"this\" \"one\"))" `shouldBe` "(quote (1 3 (\"this\" \"one\")))"