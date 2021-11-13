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

skip :: Parser String
skip = spaces

-- >>> runParser skip ""
-- [("","")]
optional p = do{ _ <- p; return ""} <|> return ""

comments :: Parser Char                                                       
comments = do 
    char ';'
    manyTill anyChar (char '\n') 
    return ';'

garbage :: Parser String
garbage = many (satisfy isSpace <|> comments)

-- >>> isSpace 
-- lexical error in string/character literal at end of input

stripComments :: Parser String 
stripComments = do 
    optional comments
    xs <- sepBy comments (manyTill anyChar (char ';'))
    optional comments
    return (concat xs)

--- >>> runParser stripComments ";; Quoting is a thing \n'x       ;; ==> 'x \n '(1 2 3)  ;; ==> (1 2 3) \n '(x y)             ;; ==> ('x 'y)"
-- [("'x        '(1 2 3)   '(x y)             ","; ==> ('x 'y)")]


-- >>> runParser comments ";; one layer of nesting \n test"
-- [(""," test")]

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
metaAST = do
    skip
    --sepBy (comments <|> skip) getNextExpr
    sepBy ( garbage ) getNextExpr

program :: Parser [Expr]
program = do
    skip 
    optional comments
    ss <- metaAST
    optional comments
    skip
    return ss


parseMeta :: [Char] -> Either [Expr] [Char]
parseMeta s = 
    case result of
        [] -> Right "invalid syntax"
        ((metaAST, "") : _) -> Left metaAST
        ((_, remaining) : _) -> Right ("invalid syntax at " ++ (take 20 remaining) ++ ".......")
        where
            result = runParser program s


goEval s  = putStrLn "Your implementation continues here"

-- >>> runParser program ";; Quoting is a thing \n 'x       ; ==> 'x \n '(1 2 3)  ; ==> (1 2 3) \n '(x y)             ; ==> ('x 'y)"
-- [([Combination [Symbol "quote",Symbol "x"],Combination [Symbol "quote",Combination [Constant 1,Constant 2,Constant 3]],Combination [Symbol "quote",Combination [Symbol "x",Symbol "y"]]],"; ==> ('x 'y)")]

-- >>> runParser program "'(1 2 3)  ; ==> (1 2 3) \n '(x y)             ; ==> ('x 'y)"
-- [([Combination [Symbol "quote",Combination [Constant 1,Constant 2,Constant 3]],Combination [Symbol "quote",Combination [Symbol "x",Symbol "y"]]],"; ==> ('x 'y)")]

-- >>> runParser program "'(x y)             ; ==> ('x 'y)"
-- [([Combination [Symbol "quote",Combination [Symbol "x",Symbol "y"]]],"; ==> ('x 'y)")]

-- >>> runParser (optional comments) ";; Quoting is a thing \n 'x                              ; ==> 'x    \n '(1 2 3)"
-- [(""," 'x                              ; ==> 'x    \n '(1 2 3)")]

-- >>> runParser program "'(1 . (2 . ()))       ; ==> (1 2) \n ;; Splicing is also a thing... I'm assuming here that you have (most of) the intrinsics implemented: \n '(1 2 $3)    ; ==> (1 2 3)"
-- [([Combination [Symbol "quote",Combination [Constant 1,Symbol ".",Combination [Constant 2,Symbol ".",Combination []]]]],"'(1 2 $3)    ; ==> (1 2 3)")]


-- >>> runParser metaAST " 'x                              ; ==> 'x    \n '(1 2 3)"
-- [([Combination [Symbol "quote",Symbol "x"],Combination [Symbol "quote",Combination [Constant 1,Constant 2,Constant 3]]],"")]
