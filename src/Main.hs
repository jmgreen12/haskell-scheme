module Main where
import System.Environment
import Control.Monad
import SchemeParser (readExpr)
import SchemeEval (eval)
import LispError

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
