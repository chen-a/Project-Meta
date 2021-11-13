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

skip = spaces

optional p = do{ _ <- p; return ()} <|> return ()

comments = do 
    char ';'
    manyTill anyChar (char '\n') 
    return ""

-- >>> runParser comments ";; one layer of nesting \n test"
-- [(""," test")]

-- >>> runParser comments "test"

-- unused atm
newLine = do
    x <- satisfy (== '\\')
    y <- satisfy (== 'n')
    return ([x] ++ [y])

anyChar = satisfy (const True)

manyTill p end = scan
                where
                    scan  = do{ _ <- end; return [] }
                            <|>
                            do{ x <- p; xs <- scan; return (x:xs) }

extraSymbols :: Parser Char
extraSymbols = satisfy (== '+') <|> satisfy (== '-') <|> satisfy (== '*') <|> satisfy (== '?')  <|> satisfy (=='?') <|> satisfy (== '.')

numbers :: Parser Expr
numbers = do
    optional comments
    skip
    optional comments
    x <- int
    return (Constant x)

-- >>> runParser numbers "-1234 5"
-- [(Constant (-1234)," 5")]

bool :: Parser Expr
bool = do
    optional comments
    skip
    optional comments
    hashtag <- satisfy (== '#')
    value <- satisfy (== 't') <|> satisfy (== 'f')
    if value == 't' then return (Boolean True)
    else return (Boolean False)

-- >>> runParser bool "#f"
-- [(Boolean False,"")]


-- >>> runParser combination "(x y . z)"
-- [(Combination [Symbol "x",Symbol "y",Symbol ".",Symbol "z"],"")]



-- original
symbols = do 
    optional comments
    skip
    optional comments
    x <- many1 (letter <|> satisfy isDigit <|> extraSymbols)
    return (Symbol x)

combination :: Parser Expr
combination = do 
    optional comments
    skip
    optional comments
    symbol "(" <|> symbol "["
    xs <- sepBy skip getNextExpr
    symbol ")" <|> symbol "]"
    return (Combination xs)

getNextExpr :: Parser Expr
getNextExpr = combination <|> bool <|> numbers <|> symbols <|> quote <|> splice

quote :: Parser Expr 
quote = do 
    char '\''
    x <- getNextExpr
    return (Combination [Symbol "quote", x])

splice :: Parser Expr
splice = do
    char '$'
    x <- getNextExpr
    return (Combination [Symbol "splice", x])
-- >>> runParser quote "'(1 2 3)"
-- [(Combination [Symbol "quote",Combination [Constant 1,Constant 2,Constant 3]],"")]

 
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
printAst (Combination (x:xs)) = "(" ++ printAst x ++ " " ++ intercalate " " (map printAst xs) ++ ")"
printAst (Combination []) = "()"

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

metaAST :: Parser [Expr]
metaAST = sepBy skip getNextExpr

program :: Parser [Expr]
program = do
    skip 
    optional comments
    ss <- metaAST
    optional comments
    skip
    return ss

parseMeta s = 
    case result of
        [] -> Right "invalid syntax"
        ((metaAST, "") : _) -> Left metaAST
        ((_, remaining) : _) -> Right ("invalid syntax at " ++ (take 20 remaining) ++ ".......")
        where
            result = runParser program s


goEval s  = putStrLn "Your implementation continues here"

-- >>> runParser program "(+ 2 3) (1 2 3) [else #f] [1 2 3] ;; dots (1 . 2) (x y . z)"
-- [([Combination [Symbol "+",Constant 2,Constant 3],Combination [Constant 1,Constant 2,Constant 3],Combination [Symbol "else",Boolean False],Combination [Constant 1,Constant 2,Constant 3]],";; dots (1 . 2) (x y . z)")]
