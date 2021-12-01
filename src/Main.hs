module Main where

import Control.Monad
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Bird.Parser
import Bird.Printer
import Data.List
import Data.Char
import Data.Bits (Bits(xor))

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
getNextExpr = combination <|> bool <|> numbers <|> symbols <|> getQuote <|> getSplice

getQuote :: Parser Expr
getQuote = do
    char '\''
    x <- getNextExpr
    return (Combination [Symbol "quote", x])

getSplice :: Parser Expr
getSplice = do
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
    sepBy (garbage) getNextExpr

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

goEval s  = do
     -- let ns = intercalate " " (lines s)
     let result = parseMeta s
     case result of
        Left metaAST -> mapM_ putStrLn (map eval metaAST)
        Right err -> putStrLn ("error: " ++ err) 

        {-do
     let parseResult = parseMeta s
     case parseResult of
        Left metaAST -> do
            let evalResult = map evalMeta metaAST
            case evalResult of
                Left something -> map putStrLn (map printAst something)
                Right err -> putStrLn ("Eval error:" ++ err)
        Right err -> putStrLn ("Parse error: " ++ err) -}

eval :: Expr -> String
eval (Boolean b)
    | b = "#t"
    | otherwise = "#f"
eval (Constant n) = show n
eval (Symbol s) = s
eval (Combination x) = combinationEval x

-- >>> runParser program "(add (add 1 2) (add (add 1 2) (add 2 3)))"
-- [([Combination [Symbol "add",Combination [Symbol "add",Constant 1,Constant 2],Combination [Symbol "add",Combination [Symbol "add",Constant 1,Constant 2],Combination [Symbol "add",Constant 2,Constant 3]]]],"")]

-- >>> combinationEval [Symbol "add",Combination [Symbol "add",Constant 1,Constant 2],Combination [Symbol "add",Combination [Symbol "add",Constant 1,Constant 2],Combination [Symbol "add",Constant 2,Constant 3]]]
-- /home/vscode/github-classroom/Iowa-CS-3820-Fall-2021/project-meta-meta-team/src/Main.hs:257:1-51: Non-exhaustive patterns in function add

-- >>> eval (Combination [Symbol "quote",Combination [Constant 1,Constant 2,Constant 3]])
-- "(1 2 3)"

-- >>> eval (Combination [Symbol "cons",Constant 1,Constant 2])
-- "(1 . 2)"

combinationEval :: [Expr] -> String -- currently doesn't report errors
combinationEval [Combination x] = combinationEval x -- not tested
combinationEval (Combination x : [xs]) = combinationEval x ++ " " ++ eval xs -- not tested
combinationEval ((Symbol s) : xs)
    --intrinsics
    | s == "eq?" = eval (equality xs)
    | s == "add" = eval (add xs)
    | s == "sub" = eval (sub xs)
    | s == "mul" = eval (mult xs)
    | s == "div" = eval (divide xs)
    | s == "cons" = addParens (consCombine (cons xs))
    | s == "fst" = eval (first xs)
    | s == "snd" = eval (second xs)
    | s == "number?" = eval (number xs)
    | s == "pair?" = eval (pair xs)
    | s == "list?" = undefined
    | s == "function?" = undefined
    -- others
    | s == "quote" = addParens (quote xs)
    | s == "splice" = splice xs
    where
            addParens x = "(" ++ x ++ ")"
            consCombine [e1, e2] = eval e1 ++ " . " ++ eval e2

equality :: [Expr] -> Expr
equality [Constant e1, Constant e2]
    | e1 == e2 = Boolean True
    | otherwise = Boolean False
equality [Boolean e1, Boolean e2]
    | e1 == e2 = Boolean True
    | otherwise = Boolean False

add, sub, mult, divide :: [Expr] -> Expr
add [Constant e1, Constant e2] = Constant (e1 + e2)
sub [Constant e1, Constant e2] = Constant (e1 - e2)
mult [Constant e1, Constant e2] = Constant (e1 * e2)
divide [Constant e1, Constant e2] = Constant (e1 `div` e2)

cons :: [Expr] -> [Expr]
cons [Constant e1, Constant e2] = [Constant e1, Constant e2]
cons [Boolean e1, Boolean e2] = [Boolean e1, Boolean e2]

first, second, number :: [Expr] -> Expr
first [Combination x] = a x
    where
        a [Symbol "cons", Constant e1, Constant e2] = Constant e1
        a [Symbol "cons", Boolean e1, Boolean e2] =  Boolean e1
        a [Constant e1, Constant e2] = Constant e1
        a [Symbol "quote", Combination y] = first [Combination y]
       

-- >>> runParser program "(fst '(1 2))"
-- [([Combination [Symbol "quote",Combination [Constant 1,Constant 2]]],"")]

-- >>> first [Combination [Constant 1,Constant 2]]
-- Constant 1

-- >>> first [Combination [Symbol "quote",Combination [Constant 1,Constant 2]]]
-- Constant 1

second [Combination x] = a x   
    where
        a [Symbol "cons", Constant e1, Constant e2] = Constant e2
        a [Symbol "cons", Boolean e1, Boolean e2] = Boolean e2

number [Constant e] = Boolean True
number [Boolean e] = Boolean False
number [Symbol e] = Boolean False
number [Combination e] = Boolean False

-- >>> runParser program "(pair? '(1 2))"
-- [([Combination [Symbol "pair?",Combination [Symbol "quote",Combination [Constant 1,Constant 2]]]],"")]

-- >>> pair ()

pair :: [Expr] -> Expr
pair [Constant e] = Boolean False 
pair [Boolean e] = Boolean False
-- pair [Combination e] = pair (combinationEval [Combination e])


quote :: [Expr] -> String -- currently doesn't evaluate levels
quote [Constant x] = show x
quote (Constant x : xs) = show x ++ " " ++ quote xs
quote [Symbol x] = x
quote (Symbol x : xs) = x ++ " " ++ quote xs
quote [Combination xs] = quote xs

-- >>> quote ([Combination [Constant 1,Constant 2,Constant 3]])
-- "1 2 3"

splice :: [Expr] -> String -- currently doesn't evaluate levels
splice [Constant x] = show x
splice [Combination xs] = combinationEval xs

-- >>> runParser program "'(x $(add 2 3) y)"
-- [([Combination [Symbol "quote",Combination [Symbol "x",Combination [Symbol "splice",Combination [Symbol "add",Constant 2,Constant 3]],Symbol "y"]]],"")]

-- >>> combinationEval [Symbol "add",Constant 2,Constant 3]
-- "5"

-- >>> splice ([Constant 5])
-- "5"

-- >>> splice ([Combination [Symbol "add",Constant 2,Constant 3]])
-- "5"


-- stuff that might help with library------------------
-- add [] = 0
-- add (Constant x : xs) = x + add xs -- currently doesn't evaluate nested adds
-- add [Combination x : xs] = combinationEval x ++ add xs
-- sub [] = 0
-- sub (Constant x : xs) = x - sub xs
-- mult [] = 1
-- mult (Constant x : xs) = x * sub xs
-- divide [] = 1
-- divide (Constant x : xs) = x `div` sub xs -- if only one digit, return (1 / x)
