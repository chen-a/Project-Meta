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


letter :: Parser Char 
letter = satisfy isAlpha

-- not needed?
whitespace = spaces <|> many1 (satisfy (== '\n'))

newLine = do
    x <- satisfy (== '\\')
    y <- satisfy (== 'n')
    return ([x] ++ [y])

operations :: Parser Char
operations = satisfy (== '+') <|> satisfy (== '-') <|> satisfy (== '*')

numbers :: Parser Expr
numbers = do
    whitespace
    x <- int
    return (Constant x)

-- >>> runParser numbers "-1234 5"
-- [(Constant (-1234)," 5")]

bool :: Parser Expr
bool = do
    whitespace
    hashtag <- satisfy (== '#')
    value <- satisfy (== 't') <|> satisfy (== 'f')
    if value == 't' then return (Boolean True)
    else return (Boolean False)

-- >>> runParser bool "#f"
-- [(Boolean False,"")]


-- >>> runParser symbols2 "'"
-- [(Symbol "quote","")]

-- >>> runParser combination "(add '1 3)"
-- [(Combination [Symbol "add",Symbol "quote",Constant 1,Constant 3],"")]


-- original
symbols = do 
    whitespace
    x <- many1 (letter <|> satisfy isDigit <|> operations)
    return (Symbol x)

symbols2 :: Parser Expr
symbols2 = do 
    x <- token next
    if (x == '\'')
        then return (Symbol "quote")
    else if (x == '$')
        then return (Symbol "splice")
    else do
        y <- many1 (letter <|> satisfy isDigit <|> operations)
        return (Symbol (x:y))    

symbols3 :: Parser Expr
symbols3 = do
    x <- token (many1 (letter <|> satisfy isDigit <|> operations))
    if (x == "\'")
        then return (Symbol "quote")
    else if (x == "$")
        then return (Symbol "splice")
    else do
        return (Symbol x)

combination :: Parser Expr
combination = do 
    whitespace
    satisfy (== '(') <|> (satisfy (== '['))
    xs <- sepBy whitespace getNextExpr
    satisfy (== ')') <|> satisfy (== ']')
    return (Combination xs)

getNextExpr :: Parser Expr
getNextExpr = combination <|> bool <|> numbers <|> symbols

-- >>> runParser combination "((lambda args args)  a )    "
-- [(Combination [Combination [Symbol "lambda",Symbol "args",Symbol "args"],Symbol "a"],"    ")]

-- >>> runParser combination "(add 1 2)"
-- [(Combination [Symbol "add",Constant 1,Constant 2],"")]

-- >>> runParser getNextExpr "1;123"
-- [(Constant 1,";123")]

-- >>> runParser combination "(+ 3)"
-- [(Combination [Symbol "+",Constant 3],"")]

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

main :: IO ()
main = do args <- getArgs
          let files = filter ("-p" /=) args
              go | elem "-p" args = goParse
                 | otherwise      = goEval
          when (not (null files)) $
              go  . concat =<< mapM readFile files

goParse, goEval :: String -> IO ()
goParse s = do
     -- let ns = intercalate " " (lines s)
     let result = parseMeta s
     case result of
        Left metaAST -> mapM_ putStrLn (map printAst metaAST)
        Right err -> putStrLn ("error: " ++ err)

metaAST = sepBy whitespace getNextExpr

program :: Parser [Expr]
program = do
    whitespace 
    ss <- metaAST
    whitespace 
    return ss

parseMeta s = 
    case result of
        [] -> Right "invalid syntax"
        ((metaAST, "") : _) -> Left metaAST
        ((_, remaining) : _) -> Right ("invalid syntax at " ++ (take 20 remaining) ++ ".......")
        where
            result = runParser program s


goEval s  = putStrLn "Your implementation continues here"
