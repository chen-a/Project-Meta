module Main where

import Control.Monad
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Bird.Parser
import Bird.Printer
import Data.List
import Data.Char
import Data.Bits (Bits(xor))
import Control.Concurrent (yield)

data Expr = Boolean Bool
    | Constant Int
    | Symbol String
    | Combination [Expr]
    | Dot [Expr] Expr
    deriving Show


letter :: Parser Char
letter = satisfy isAlpha

skip :: Parser String
skip = spaces

optional p = do{ _ <- p; return ""} <|> return ""

comments :: Parser Char
comments = do
    char ';'
    manyTill anyChar (char '\n')
    return ';'

garbage :: Parser String
garbage = many (satisfy isSpace <|> comments)

stripComments :: Parser String
stripComments = do
    optional comments
    xs <- sepBy comments (manyTill anyChar (char ';'))
    optional comments
    return (concat xs)

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

bool :: Parser Expr
bool = do
    optional comments
    skip
    optional comments
    hashtag <- satisfy (== '#')
    value <- satisfy (== 't') <|> satisfy (== 'f')
    if value == 't' then return (Boolean True)
    else return (Boolean False)

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
     let result = parseMeta s
     case result of
        Left metaAST -> mapM_ putStrLn (map printAst metaAST)
        Right err -> putStrLn ("error: " ++ err)

metaAST :: Parser [Expr]
metaAST = do
    skip
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
     let result = parseMeta s
     case result of
        -- Left metaAST -> mapM_ putStrLn (map printEval (map eval metaAST))
        Left metaAST -> mapM_ putStrLn (map printEval (map getExpr result))
            where 
                getExpr :: (Expr, Environment) -> Expr
                getExpr (expr, env) = expr 
                result = map (\x -> eval x env) metaAST
                    where
                        env = Env []
        Right err -> putStrLn ("error: " ++ err)

newtype Environment = Env [(String, Expr)] deriving Show

envLookup :: Environment -> String -> Maybe Expr
envLookup (Env []) s = Nothing
envLookup (Env ((vname,value) : rest)) s = 
    if vname == s then Just value 
                  else envLookup (Env rest) s

envAdd :: Environment -> (String, Expr) -> Environment
envAdd (Env ps) p = Env (p : ps) 

getExpr :: (Expr, Environment) -> Expr
getExpr (expr, env) = expr 

test_env1, test_env2 :: Environment
test_env1 = Env [("v2", Constant 88), ("v2", Constant 23), ("v1", Constant 10)]
test_env2 = Env [("v2", Constant 8888), ("v2", Constant 23), ("v1", Constant 10)]

-- >>> envLookup test_env1 "v2"
-- Just (Constant 8888)

-- Error Statement
errUndefVar :: String -> String 
errUndefVar s = "undefined variable: '" ++ s ++ "'"

printEval :: Expr -> String 
printEval (Boolean b)
    | b = "#t"
    | otherwise = "#f"
printEval (Constant n) = show n
printEval (Symbol s) = s
printEval (Combination [Symbol "splice", x]) = printEval x
printEval (Combination xs) = "(" ++ printCombine xs ++ ")"
    where
        printCombine [] = ""
        printCombine [e] = printEval e
        printCombine (e:es) = printEval e ++ " " ++ printCombine es 
printEval (Dot xs x) = "(" ++ printCombine xs ++ " . " ++ printEval x ++ ")"
     where
        printCombine [] = ""
        printCombine [e] = printEval e
        printCombine (e:es) = printEval e ++ " " ++ printCombine es 

printCombo :: Expr -> String
printCombo (Boolean b) = printEval (Boolean b)
printCombo (Constant n) = printEval (Constant n)
printCombo (Symbol s) = printEval (Symbol s)
printCombo (Combination (Symbol "quote" : y)) = "(" ++ quoteCombine y ++ ")"
    where
        quoteCombine [] = ""
        quoteCombine [e] = printCombo e
        quoteCombine (e1:e2) = quoteCombine [e1] ++ " " ++ quoteCombine e2
printCombo (Combination (Symbol "consNil" : y)) = "(" ++ consNil y ++ ")"
    where
        consNil [] = ""
        consNil [e] = printEval e
        consNil (e1:e2) = consNil [e1] ++ " " ++ consNil e2
printCombo (Combination (Symbol "consPair" : y)) = "(" ++ consPair y ++ ")"
    where
        consPair [e1, e2] = printEval e1 ++ " . " ++ printEval e2
        consPair (e1: e2) = printEval e1 ++ " " ++ consPair e2

eval :: Expr -> Environment -> (Expr, Environment)
eval (Boolean b) env = (Boolean b, env)
eval (Constant n) env = (Constant n, env)
eval (Symbol s) env = (Symbol s, env)
eval (Combination x) env =  combinationEval x env

combinationEval :: [Expr] -> Environment -> (Expr, Environment) -- currently doesn't report errors?
combinationEval [Constant x] env = (Constant x, env)
combinationEval [Combination x] env = combinationEval x env
combinationEval ((Symbol s) : xs) env
    --intrinsics
    |  s == "eq?" = equality xs env
    | s == "add" = add xs env
    | s == "sub" = sub xs env
    | s == "mul" = mult xs env
    | s == "div" = divide xs env
    | s == "cons" = cons xs env
    | s == "fst" = first xs env
    | s == "snd" = second xs env
    | s == "number?" = number xs env
    | s == "pair?" = pair xs env
    | s == "list?" = list xs env
    | s == "function?" = function xs env
    -- special forms
    | s == "quote" = quote xs env
    | s == "splice" = splice xs env
    | s == "if" = conditional xs env


-- >>> runParser program "(cons 1 2)"
-- [([Combination [Symbol "cons",Constant 1,Constant 2]],"")]

-- >>> combinationEval [Symbol "cons",Constant 1,Constant 2]
-- [Constant 1,Constant 2]

equality :: [Expr] -> Environment-> (Expr, Environment)
equality [Constant e1, Constant e2] env 
    | e1 == e2 = (Boolean True, env)
    | otherwise = (Boolean False, env)
equality [Boolean e1, Boolean e2] env
    | e1 == e2 = (Boolean True, env)
    | otherwise = (Boolean False, env)
equality [Constant e1, Combination (Symbol "add":xs)] env = equality [Constant e1, getExpr (add xs env)] env
equality [Combination (Symbol "add":xs), Constant e2] env = equality [getExpr (add xs env), Constant e2] env

add, sub, mult, divide :: [Expr] -> Environment -> (Expr, Environment)

add [Constant e1, Constant e2] env = (Constant (e1 + e2), env)
add [Constant e1, Combination e2] env = add [Constant e1, getExpr (eval (Combination e2) env)] env
add [Combination e1, Constant e2] env = add [getExpr (eval (Combination e1) env), Constant e2] env
add [Combination e1, Combination e2] env = add [getExpr (eval (Combination e1) env), getExpr (eval (Combination e2) env)] env
    
-- add [Combination x] env = eval (Combination x)

sub [Constant e1, Constant e2] env = (Constant (e1 - e2), env)
sub [Combination ((Symbol x):xs)] env = sub xs env
sub [Constant e1, Combination e2] env = sub [Constant e1, getExpr (eval (Combination e2) env)] env
sub [Combination e1, Constant e2] env = sub [getExpr (eval (Combination e1) env), Constant e2] env
sub [Combination e1, Combination e2] env = sub [getExpr (eval (Combination e1) env), getExpr (eval (Combination e2) env)] env
    

mult [Constant e1, Constant e2] env = (Constant (e1 * e2), env)
divide [Constant e1, Constant e2] env = (Constant (e1 `div` e2), env)

cons :: [Expr] -> Environment -> (Expr, Environment)
cons [x, Symbol "nil"] env = (Combination [getExpr (eval x env)], env)
cons [x, Combination (Symbol "cons" : xs)] env = cons [getExpr (eval x env), getExpr (cons xs env)] env
cons [x, Combination xs] env = (Combination (getExpr(eval x env) : xs), env)
cons [x, Dot ys y] env = (Dot (getExpr (eval x env) : ys) y, env) 
cons [x, y] env = (Dot [getExpr (eval x env)] y, env)
cons _ env = (Symbol "Error", env)

-- >>> runParser program "(cons (add 1 (add 3 (add 4 5))) (cons 1 (cons 3 (cons (add 2 (add 3 4)) nil))))"
-- [([Combination [Symbol "cons",Combination [Symbol "add",Constant 1,Combination [Symbol "add",Constant 3,Combination [Symbol "add",Constant 4,Constant 5]]],Combination [Symbol "cons",Constant 1,Combination [Symbol "cons",Constant 3,Combination [Symbol "cons",Combination [Symbol "add",Constant 2,Combination [Symbol "add",Constant 3,Constant 4]],Symbol "nil"]]]]],"")]

-- >>> cons [Combination [Symbol "add",Constant 1,Combination [Symbol "add",Constant 3,Combination [Symbol "add",Constant 4,Constant 5]]],Combination [Symbol "cons",Constant 1,Combination [Symbol "cons",Constant 3,Combination [Symbol "cons",Combination [Symbol "add",Constant 2,Combination [Symbol "add",Constant 3,Constant 4]], Constant 1]]]]
-- Dot [Constant 13,Constant 1,Constant 3,Constant 9] (Constant 1)

-- >>> cons [Constant 3,Combination [Symbol "cons",Combination [Symbol "add",Constant 2,Combination [Symbol "add",Constant 3,Constant 4]], Constant 1]]
-- Dot [Constant 3,Constant 9] (Constant 1)

-- >>> cons [Combination [Symbol "add",Constant 2,Constant 7], Constant 1]
-- Dot [Constant 9] (Constant 1)

-- >>> eval (Combination [Symbol "add",Constant 2,Constant 7])
-- Constant 9

-- >>> cons [Constant 1,Combination [Symbol "cons",Constant 3,Combination [Symbol "cons",Combination [Symbol "add",Constant 2,Combination [Symbol "add",Constant 3,Constant 4]],Symbol "nil"]]]
-- Combination [Constant 1,Constant 3,Constant 9]

first, second, number :: [Expr] -> Environment -> (Expr, Environment)
first [Combination x] env = a x
    where
        a [Symbol "cons", e1, e2] = (e1, env)
        a [Constant e1, Constant e2] = (Constant e1, env)
        a [Symbol "quote", Combination y] = first [Combination y] env

second [Combination x] env = a x
    where
        a [Symbol "cons", e1, e2] = (e2, env)
        a [Boolean e1, Boolean e2] = (Boolean e2, env)

number [Constant e] env = (Boolean True, env)
number [Boolean e] env = (Boolean False, env)
number [Symbol e] env = (Boolean False, env)
number [Combination e] env = (Boolean False, env)

pair :: [Expr] -> Environment -> (Expr, Environment)
pair [Constant e] env = (Boolean False, env)
pair [Boolean e] env = (Boolean False, env)
pair [Constant e1, Constant e2] env = (Boolean True, env)
pair [Boolean e1, Boolean e2] env = (Boolean True, env)
pair [Combination x] env = pair x env
pair (Symbol x : ys) env = pair ys env


list :: [Expr] -> Environment -> (Expr, Environment)
list [Constant e] env = (Boolean False, env)
list [Boolean e] env= (Boolean False, env)
list [Symbol e] env = (Boolean False, env)
list (x:xs) env = (Boolean True, env)

function :: [Expr] -> Environment -> (Expr, Environment)
function [Symbol e] env = if e `elem` flist
    then (Boolean True, env)
    else (Boolean False, env)
        where flist = ["eq?", "add", "sub", "mul", "div", "cons", "fst", "snd", "number?", "pair?", "list?", "function?"]
function [Constant e] env = (Boolean False, env)
function [Boolean e] env = (Boolean False, env)
function [Combination e] env = (Boolean False, env)


-- quote [Constant 1, Constant 2]
-- Combination [Symbol "quote", Constant 1, Combination [Symbol "quote", Constant 2]]

quote :: [Expr] -> Environment -> (Expr, Environment)
quote [Constant x] env = (Constant x, env)
quote [Symbol s] env = (Symbol s, env)
quote [Combination xs] env = (Combination xs, env)
quote [Dot xs x] env = (Dot xs x, env)

-- >>> quote [Combination [Constant 1, Constant 2]]
-- Combination [Symbol "quote",Combination [Constant 1,Constant 2]]

splice :: [Expr] -> Environment -> (Expr, Environment)
splice [Constant x] env = (Constant x, env)
splice [Symbol s] env = (Symbol s, env)
splice [Combination xs] env = combinationEval xs env

conditional :: [Expr] -> Environment -> (Expr, Environment)
conditional [Boolean True, x, y] env = (x, env)
conditional [Boolean False, x, y] env = (y, env)


-- >>> runParser program "'(1 2 $3)"
-- [([Combination [Symbol "quote",Combination [Constant 1,Constant 2,Combination [Symbol "splice",Constant 3]]]],"")]

-- >>> quote [Combination [Constant 1,Constant 2,Combination [Symbol "splice",Constant 3]]]
-- /home/vscode/github-classroom/Iowa-CS-3820-Fall-2021/project-meta-meta-team/src/Main.hs:(309,9)-(310,40): Non-exhaustive patterns in function multiquote

-- >>> runParser program "'(x y . z)"
-- [([Combination [Symbol "quote",Combination [Symbol "x",Symbol "y",Symbol ".",Symbol "z"]]],"")]

-- >>> quote [Combination [Symbol "x",Symbol "y",Symbol ".",Symbol "z"]]
-- Combination [Symbol "quote",Symbol "x",Symbol "y",Symbol ".",Symbol "z"]

-- >>> printCombo (Combination [Symbol "quote",Symbol "x",Symbol "y",Symbol ".",Symbol "z"])
-- "(x y . z)"

-- >>> cons [Symbol "x",Symbol "y",Symbol ".",Symbol "z"]
-- No instance for (Show ([Expr] -> Expr))
--   arising from a use of ‘evalPrint’
--   (maybe you haven't applied a function to enough arguments?)

-- >>> runParser program "'(x y . z)"
-- [([Combination [Symbol "quote",Combination [Symbol "x",Symbol "y",Symbol ".",Symbol "z"]]],"")]

-- >>> quote [Combination [Symbol "x",Symbol "y",Symbol ".",Symbol "z"]]
-- Combination [Symbol "quote",Symbol "x",Symbol "y",Symbol ".",Symbol "z"]

-- >>> printCombo (Combination [Symbol "quote",Constant 1,Constant 2,Constant 3])
-- "(1 2 3)"

-- >>> quote [Combination [Symbol "x", Symbol "y", Symbol ".", Symbol "z"]]
-- Combination [Symbol "quote",Symbol "x",Symbol "y",Symbol ".",Symbol "z"]

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
