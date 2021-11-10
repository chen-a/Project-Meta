module Main where

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
-- import Bird.Parser
-- import Bird.Printer
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
data Expr = Boolean Bool 
    | Integer Integer 
    | Symbols String
    | Pairs Expr Expr
    | Combinations [Expr]

main :: IO ()
main = do args <- getArgs
          let files = filter ("-p" /=) args
              go | elem "-p" args = goParse
                 | otherwise      = goEval
          when (not (null files)) $
              go  . concat =<< mapM readFile files

goParse, goEval :: String -> IO ()
goParse s = putStrLn "Your implementation begins here"              
goEval s  = putStrLn "Your implementation continues here"

numbers :: GenParser Char st Int
numbers = do
    spaces
    x <- many1 digit
    return (read x :: Int)

word :: GenParser Char st String
word = do
    spaces 
    x <- many1 letter 
    return x

