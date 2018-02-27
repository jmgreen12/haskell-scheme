module Main where
import System.Environment
import SchemeParser (readExpr, eval)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
