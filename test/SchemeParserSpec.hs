module SchemeParserSpec (main, spec) where

import Test.Hspec
import SchemeParser (readExpr)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "readExpr" $ do
        it "parses a string" $ do
            (show . readExpr) "\"abcd\"" `shouldBe` "\"abcd\""
        
        it "parses a string with escaped quotes" $ do
            (show . readExpr) "\"abc\\\"d\"" `shouldBe` "\"abc\"d\""

        it "parses a string with escaped characters" $ do
            (show . readExpr) "\"\\ttab\\nnewline\\rreturn\\\\slash\"" `shouldBe` "\"\ttab\nnewline\rreturn\\slash\""
        
        it "parses a list" $ do
            (show . readExpr) "(1 2 2)" `shouldBe` "(1 2 2)"

        it "parses a quoted literal" $ do
            (show . readExpr) "'(1 3 (\"this\" \"one\"))" `shouldBe` "(quote (1 3 (\"this\" \"one\")))"