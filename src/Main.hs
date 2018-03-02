module Main where
import System.Environment
import SchemeParser (readExpr)
import SchemeEval (eval)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
