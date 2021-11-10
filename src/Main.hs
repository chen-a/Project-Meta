module Main where

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Bird.Parser
import Bird.Printer
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


numbers :: Parser [Int]
numbers = many digit

