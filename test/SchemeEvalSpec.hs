module SchemeEvalSpec (main, spec) where

import Test.Hspec
import SchemeParser (readExpr)
import SchemeEval (eval)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "eval" $ do
        it "evaluates addition" $ do
            (show . eval . readExpr) "(+ 2 2)" `shouldBe` "4"
        
        it "evaluates nested addition and subtraction" $ do
            (show . eval .readExpr) "(- (+ 4 6 3) 3 5 2)" `shouldBe` "3"