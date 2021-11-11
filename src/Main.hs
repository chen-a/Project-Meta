module Main where

import Control.Monad 
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Bird.Parser
import Bird.Printer
import Data.List
import Data.Char

data Expr = Boolean Bool 
    | Constant Int 
    | Symbol String
    | Combination [Expr]
    deriving Show

main :: IO ()
main = do args <- getArgs
          let files = filter ("-p" /=) args
              go | elem "-p" args = goParse
                 | otherwise      = goEval
          when (not (null files)) $
              go  . concat =<< mapM readFile files

goParse, goEval :: String -> IO ()
goParse s = putStrLn (runParser getNextExpr s)



goEval s  = putStrLn "Your implementation continues here"

letter :: Parser Char 
letter = satisfy isAlpha

operations :: Parser Char
operations = satisfy (== '+') <|> satisfy (== '-') <|> satisfy (== '*')

numbers :: Parser Expr
numbers = do
    spaces
    x <- int
    return (Constant x)

-- >>> runParser numbers "1234 5"
-- [(Constant 1234," 5")]

bool :: Parser Expr
bool = do
    spaces 
    hashtag <- satisfy (== '#')
    value <- satisfy (== 't') <|> satisfy (== 'f')
    if value == 't' then return (Boolean True)
    else return (Boolean False)

-- >>> runParser bool "#f"
-- [(Boolean False,"")]

symbols :: Parser Expr
symbols = do
    spaces 
    x <- many1 (letter <|> satisfy isDigit <|> operations)
    return (Symbol x)

-- >>> runParser symbols "              wrd"
-- [(Symbol "wrd","")]

combination :: Parser Expr
combination = do 
    spaces
    satisfy (== '(') <|> satisfy (== '[')
    xs <- sepBy spaces getNextExpr
    satisfy (== ')') <|> satisfy (== ']')
    return (Combination xs)

getNextExpr :: Parser Expr
getNextExpr = combination <|> bool <|> numbers <|> symbols
-- >>> runParser combination "(add 1 3)"
-- [(Combination [Symbol "add",Constant 1,Constant 3],"")]

-- >>> runParser combination "(+ 2 3)"
-- [(Combination [Symbol "+",Constant 2,Constant 3],"")]

-- >>> runParser combination "[]"
-- [(Combination [],"")]

-- Prints the AST
printAst :: Expr -> String
printAst (Boolean b)
    | b = "#t"
    | otherwise = "#f"
printAst (Constant n) = show n
printAst (Symbol s) = s
printAst (Combination (x:xs)) = "(" ++ intercalate " " (map printAst xs) ++ ")"
printAst (Combination []) = ""
