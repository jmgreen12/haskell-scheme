module SchemeEvalSpec (main, spec) where

import Control.Monad
import LispError (extractValue)
import SchemeParser (readExpr)
import SchemeEval (eval)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "eval" $ do
        it "evaluates addition" $ do
            (show $ extractValue $ (eval <=< readExpr) "(+ 2 2)") `shouldBe` "4"
        
        it "evaluates nested addition and subtraction" $ do
            (show $ extractValue $ (eval <=< readExpr) "(- (+ 4 6 3) 3 5 2)") `shouldBe` "3"