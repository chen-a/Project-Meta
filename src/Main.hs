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
        Left metaAST -> mapM_ putStrLn (map printEval (map eval metaAST))
        Right err -> putStrLn ("error: " ++ err)


newtype Environment = Env [(String, Expr)] deriving Show

envLookup :: Environment -> String -> Maybe Expr
envLookup (Env []) s = Nothing
envLookup (Env ((vname,value) : rest)) s = 
    if vname == s then Just value 
                  else envLookup (Env rest) s

envAdd :: Environment -> (String, Expr) -> Environment
envAdd (Env ps) p = Env (p : ps) 

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

eval :: Expr -> Expr
eval (Boolean b) = Boolean b
eval (Constant n) = Constant n
eval (Symbol s) = Symbol s
eval (Combination x) =  combinationEval x

combinationEval :: [Expr] -> Expr -- currently doesn't report errors?
combinationEval [Constant x] = Constant x
combinationEval [Combination x] = combinationEval x
combinationEval ((Symbol s) : xs)
    --intrinsics
    | s == "eq?" = equality xs
    | s == "add" = add xs
    | s == "sub" = sub xs
    | s == "mul" = mult xs
    | s == "div" = divide xs
    | s == "cons" = cons xs 
    | s == "fst" = first xs
    | s == "snd" = second xs
    | s == "number?" = number xs
    | s == "pair?" = pair xs
    | s == "list?" = list xs
    | s == "function?" = function xs
    -- special forms
    | s == "quote" = quote xs
    | s == "splice" = splice xs
    | s == "if" = conditional xs

enveval :: Expr -> Expr
enveval (Boolean b) = Boolean b
enveval (Constant n) = Constant n
enveval (Symbol s) = Symbol s
enveval (Combination x) =  combinationEval x

envcombinationEval :: [Expr] -> Environment -> (Expr, Environment) -- currently doesn't report errors?
envcombinationEval [Constant x] env = (Constant x, env)
envcombinationEval [Combination x] env = envcombinationEval x env
envcombinationEval ((Symbol s) : xs) env
    --intrinsics
    |  s == "eq?" = envequality xs env
    | s == "add" = envadd xs env
    | s == "sub" = envsub xs env
    | s == "mul" = envmult xs env
    | s == "div" = envdivide xs env
    | s == "cons" = envcons xs env
    | s == "fst" = envfirst xs env
    | s == "snd" = envsecond xs env
    | s == "number?" = envnumber xs env
    | s == "pair?" = envpair xs env
    | s == "list?" = envlist xs env
    | s == "function?" = envfunction xs env
    -- special forms
    | s == "quote" = envquote xs env
    | s == "splice" = envsplice xs env
    | s == "if" = envconditional xs env


-- >>> runParser program "(cons 1 2)"
-- [([Combination [Symbol "cons",Constant 1,Constant 2]],"")]

-- >>> combinationEval [Symbol "cons",Constant 1,Constant 2]
-- [Constant 1,Constant 2]

equality :: [Expr] -> Expr
equality [Constant e1, Constant e2]
    | e1 == e2 = Boolean True
    | otherwise = Boolean False
equality [Boolean e1, Boolean e2]
    | e1 == e2 = Boolean True
    | otherwise = Boolean False
equality [Constant e1, Combination (Symbol "add":xs)] = equality [Constant e1, add xs]
equality [Combination (Symbol "add":xs), Constant e2] = equality [add xs, Constant e2]

envequality :: [Expr] -> Environment-> (Expr, Environment)
envequality [Constant e1, Constant e2] env 
    | e1 == e2 = (Boolean True, env)
    | otherwise = (Boolean False, env)
envequality [Boolean e1, Boolean e2] env
    | e1 == e2 = (Boolean True, env)
    | otherwise = (Boolean False, env)
envequality [Constant e1, Combination (Symbol "add":xs)] env = envequality [Constant e1, add xs] env
envequality [Combination (Symbol "add":xs), Constant e2] env = envequality [add xs, Constant e2] env


envadd, envsub, envmult, envdivide :: [Expr] -> Environment -> (Expr, Environment)

envadd [Constant e1, Constant e2] env = (Constant (e1 + e2), env)
envadd [Constant e1, Combination e2] env = envadd [Constant e1, eval (Combination e2)] env
envadd [Combination e1, Constant e2] env = envadd [eval (Combination e1), Constant e2] env
envadd [Combination e1, Combination e2] env = envadd [eval (Combination e1), eval (Combination e2)] env
-- envadd [Combination x] env = eval (Combination x)

envsub [Constant e1, Constant e2] env = (Constant (e1 - e2), env)
envsub [Combination ((Symbol x):xs)] env = envsub xs env
envsub [Constant e1, Combination e2] env = envsub [Constant e1, eval (Combination e2)] env
envsub [Combination e1, Constant e2] env = envsub [eval (Combination e1), Constant e2] env
envsub [Combination e1, Combination e2] env = envsub [eval (Combination e1), eval (Combination e2)] env

envmult [Constant e1, Constant e2] env = (Constant (e1 * e2), env)
envdivide [Constant e1, Constant e2] env = (Constant (e1 `div` e2), env)





add, sub, mult, divide :: [Expr] -> Expr
add [Constant e1, Constant e2] = Constant (e1 + e2)
add [Constant e1, Combination e2] = add [Constant e1, eval (Combination e2)]
add [Combination e1, Constant e2] = add [eval (Combination e1), Constant e2]
add [Combination e1, Combination e2] = add [eval (Combination e1), eval (Combination e2)]
add [Combination x] = eval (Combination x)
-- add [Symbol x, Symbol y] = add (lookup x) (lookup y)


sub [Constant e1, Constant e2] = Constant (e1 - e2)
sub [Combination ((Symbol x):xs)] = sub xs
sub [Constant e1, Combination e2] = sub [Constant e1, eval (Combination e2)]
sub [Combination e1, Constant e2] = sub [eval (Combination e1), Constant e2]
sub [Combination e1, Combination e2] = sub [eval (Combination e1), eval (Combination e2)]

mult [Constant e1, Constant e2] = Constant (e1 * e2)
divide [Constant e1, Constant e2] = Constant (e1 `div` e2)

envcons :: [Expr] -> Environment -> (Expr, Environment)
envcons [x, Symbol "nil"] env = (Combination [eval x], env)
envcons [x, Combination (Symbol "cons" : xs)] env = envcons [eval x, cons xs] env
envcons [x, Combination xs] env = (Combination (eval x:xs), env)
envcons [x, Dot ys y] env = (Dot (eval x:ys) y, env) 
envcons [x, y] env = (Dot [eval x] y, env)
envcons _ env = (Symbol "Error", env)

cons2 [Constant e, Symbol "nil"] = Combination [Symbol "consNil", Constant e]
cons2 [Combination e, Symbol "nil"] = Combination [Symbol "consNil", eval (Combination e)]
cons2 [Constant e1, Constant e2] = Combination [Symbol "consPair", Constant e1, Constant e2]
cons2 [Boolean e1, Boolean e2] = Combination [Symbol "consPair", Boolean e1, Boolean e2]
cons2 [Constant e1, Combination (Symbol "consNil": xs)] = Combination ([Symbol "consNil", Constant e1] ++ xs)
cons2 [Constant e1, Combination (Symbol "consPair": xs)] = Combination ([Symbol "consPair", Constant e1] ++ xs)
cons2 [Constant e1, Combination (Symbol "cons" : xs)] = cons [Constant e1, cons xs]
cons2 [Combination x, Combination y] = cons [eval (Combination x), eval (Combination y)]

cons :: [Expr] -> Expr
cons [x, Symbol "nil"] = Combination [eval x]
cons [x, Combination (Symbol "cons" : xs)] = cons [eval x, cons xs]
cons [x, Combination xs] = Combination (eval x:xs) 
cons [x, Dot ys y] = Dot (eval x:ys) y 
cons [x, y] = Dot [eval x] y
cons _ = Symbol "Error"

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

envfirst, envsecond, envnumber :: [Expr] -> Environment -> (Expr, Environment)
envfirst [Combination x] env = a x
    where
        a [Symbol "cons", e1, e2] = (e1, env)
        a [Constant e1, Constant e2] = (Constant e1, env)
        a [Symbol "quote", Combination y] = envfirst [Combination y] env

envsecond [Combination x] env = a x
    where
        a [Symbol "cons", e1, e2] = (e2, env)
        a [Boolean e1, Boolean e2] = (Boolean e2, env)

envnumber [Constant e] env = (Boolean True, env)
envnumber [Boolean e] env = (Boolean False, env)
envnumber [Symbol e] env = (Boolean False, env)
envnumber [Combination e] env = (Boolean False, env)


first, second, number :: [Expr] -> Expr
first [Combination x] = a x
    where
        a [Symbol "cons", e1, e2] = e1
        a [Constant e1, Constant e2] = Constant e1
        a [Symbol "quote", Combination y] = first [Combination y]

second [Combination x] = a x
    where
        a [Symbol "cons", e1, e2] = e2
        a [Boolean e1, Boolean e2] = Boolean e2

number [Constant e] = Boolean True
number [Boolean e] = Boolean False
number [Symbol e] = Boolean False
number [Combination e] = Boolean False

pair :: [Expr] -> Expr
pair [Constant e] = Boolean False
pair [Boolean e] = Boolean False
pair [Constant e1, Constant e2] = Boolean True
pair [Boolean e1, Boolean e2] = Boolean True
pair [Combination x] = pair x
pair (Symbol x : ys) = pair ys

envpair :: [Expr] -> Environment -> (Expr, Environment)
envpair [Constant e] env = (Boolean False, env)
envpair [Boolean e] env = (Boolean False, env)
envpair [Constant e1, Constant e2] env = (Boolean True, env)
envpair [Boolean e1, Boolean e2] env = (Boolean True, env)
envpair [Combination x] env = envpair x env
envpair (Symbol x : ys) env = envpair ys env


list :: [Expr] -> Expr
list [Constant e] = Boolean False
list [Boolean e] = Boolean False
list [Symbol e] = Boolean False
list (x:xs) = Boolean True

envlist :: [Expr] -> Environment -> (Expr, Environment)
envlist [Constant e] env = (Boolean False, env)
envlist [Boolean e] env= (Boolean False, env)
envlist [Symbol e] env = (Boolean False, env)
envlist (x:xs) env = (Boolean True, env)

function :: [Expr] -> Expr
function [Symbol e] = if e `elem` flist
    then Boolean True
    else Boolean False
        where flist = ["eq?", "add", "sub", "mul", "div", "cons", "fst", "snd", "number?", "pair?", "list?", "function?"]
function [Constant e] = Boolean False
function [Boolean e] = Boolean False
function [Combination e] = Boolean False

envfunction :: [Expr] -> Environment -> (Expr, Environment)
envfunction [Symbol e] env = if e `elem` flist
    then (Boolean True, env)
    else (Boolean False, env)
        where flist = ["eq?", "add", "sub", "mul", "div", "cons", "fst", "snd", "number?", "pair?", "list?", "function?"]
envfunction [Constant e] env = (Boolean False, env)
envfunction [Boolean e] env = (Boolean False, env)
envfunction [Combination e] env = (Boolean False, env)


-- quote [Constant 1, Constant 2]
-- Combination [Symbol "quote", Constant 1, Combination [Symbol "quote", Constant 2]]

quote :: [Expr] -> Expr 
quote [Constant x] = Constant x
quote [Symbol s] = Symbol s
quote [Combination xs] = Combination xs
quote [Dot xs x] = Dot xs x


envquote :: [Expr] -> Environment -> (Expr, Environment)
envquote [Constant x] env = (Constant x, env)
envquote [Symbol s] env = (Symbol s, env)
envquote [Combination xs] env = (Combination xs, env)
envquote [Dot xs x] env = (Dot xs x, env)

-- >>> quote [Combination [Constant 1, Constant 2]]
-- Combination [Symbol "quote",Combination [Constant 1,Constant 2]]

splice :: [Expr] -> Expr 
splice [Constant x] = Constant x
splice [Symbol s] = Symbol s
splice [Combination xs] = combinationEval xs

envsplice :: [Expr] -> Environment -> (Expr, Environment)
envsplice [Constant x] env = (Constant x, env)
envsplice [Symbol s] env = (Symbol s, env)
envsplice [Combination xs] env = envcombinationEval xs env

conditional :: [Expr] -> Expr
conditional [Boolean True, x, y] = x
conditional [Boolean False, x, y] = y

envconditional :: [Expr] -> Environment -> (Expr, Environment)
envconditional [Boolean True, x, y] env = (x, env)
envconditional [Boolean False, x, y] env = (y, env)


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
