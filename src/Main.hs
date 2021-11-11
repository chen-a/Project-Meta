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
goParse s = undefined --putStrLn (runParser getNextExpr s)



goEval s  = putStrLn "Your implementation continues here"

letter :: Parser Char 
letter = satisfy isAlpha

operations :: Parser Char
operations = satisfy (== '+') <|> satisfy (== '-') <|> satisfy (== '*')

numbers :: Parser Expr
numbers = do
    x <- token int
    return (Constant x)

-- >>> runParser numbers "-1234 5"
-- [(Constant (-1234)," 5")]

bool :: Parser Expr
bool = do
    hashtag <- token (satisfy (== '#'))
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
 {- symbols1 = do 
    x <- token (many1 (letter <|> satisfy isDigit <|> operations))
    return (Symbol x)
-}
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
-- >>> runParser symbols2 "+ d d"
-- []
-- >>> runParser symbols2 "$5"
-- [(Symbol "splice","5")]
-- >>> runParser combination "(f 4 4 )"
-- []

symbols3 :: Parser Expr
symbols3 = do
    x <- token (many1 (letter <|> satisfy isDigit <|> operations))
    if (x == "\'")
        then return (Symbol "quote")
    else if (x == "$")
        then return (Symbol "splice")
    else do
        return (Symbol x)
-- >>> runParser symbols3 "+ d d"
-- [(Symbol "+"," d d")]

-- >>> runParser symbols3 "$ 5"
-- []

-- >>> runParser combination "(f 4 4 )"
-- [(Combination [Symbol "f",Constant 4,Constant 4],"")]

symbols4 :: Parser Expr
symbols4 = do
    y <- satisfy (== '\'')
    z <- satisfy (== '$')
    x <- many (letter <|> satisfy isDigit <|> operations)
    if y == '\''
        then return (Symbol "quote")
    else if z == '$' 
        then return (Symbol "splice")
    else 
        return (Symbol x)

-- >>> runParser symbols4 "+ d d"
-- []
-- >>> runParser symbols4 "$ 5"
-- []
-- >>> runParser combination "(+ 4 4 )"
-- []

-- >>> runParser symbols2 "$(1 2 3) "
-- [(Symbol "splice","(1 2 3) ")]

combination :: Parser Expr
combination = do 
    token (satisfy (== '(')) <|> token (satisfy (== '['))
    xs <- sepBy spaces getNextExpr
    satisfy (== ')') <|> satisfy (== ']')
    return (Combination xs)

getNextExpr :: Parser Expr
getNextExpr = combination <|> bool <|> numbers <|> symbols3

-- >>> runParser combination "((lambda args args)  a )    "
-- []

-- >>> runParser getNextExpr "1;123"
-- [(Constant 1,";123")]

-- >>> runParser combination "(+ 3)"
-- []

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
