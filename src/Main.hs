{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Bird.Parser
import Bird.Printer
import Data.List
import Data.Char
import Data.Bits (Bits(xor))
import Data.Maybe
import Control.Concurrent (yield)

data Expr = Boolean Bool
    | Constant Int
    | Symbol String
    | Combination [Expr]
    | Dot [Expr] Expr
    | Lambda Environment [Expr] Expr
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
        --Left metaAST -> mapM_ putStrLn (map printEval (map getExpr result))
          --   where
            --     env = Env []
              --   result = map (\x -> eval x env) metaAST

        Left metaAST -> mapM_ putStrLn (map printEval (filter filterBlanks (eval metaAST env)))
            where
                env = Env []
                filterBlanks x = case x of
                                    Symbol "" -> False
                                    _ -> True

        Right err -> putStrLn ("error: " ++ err)

-- >>> filter filterBlanks [Symbol "", Symbol "5", Constant 5, Combination[Symbol "5"]]
-- [Symbol "5",Constant 5,Combination [Symbol "5"]]

-- >>> runParser program "#t \n #f"
-- [([Boolean True,Boolean False],"")]

-- >>> map printEval (eval [Boolean True,Boolean False] (Env [("k", Constant 5)]))
-- ["#t","#f"]

-- >>> eval [Boolean True] (Env [("k", Constant 5)])
-- [Boolean True]
 
newtype Environment = Env [(String, Expr)] deriving Show

envLookup :: Environment -> String -> Expr
envLookup (Env []) s = Symbol "error"
envLookup (Env ((vname,value) : rest)) s =
    if vname == s then value
                  else envLookup (Env rest) s

envLookup2 :: Environment -> Expr -> Expr
envLookup2 (Env ((vname,value) : rest)) (Symbol s) =
    if vname == s then value
                  else envLookup2 (Env rest) (Symbol s)


envAdd :: Environment -> (String, Expr) -> (Expr, Environment)
envAdd (Env ps) p = (Symbol "", Env (p : ps))

envAddMult :: Environment -> [(String, Expr)] -> Environment
envAddMult (Env ps) [ts] = Env (ts:ps) 

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

{-- 
eval :: Expr -> Environment -> (Expr, Environment)
eval (Boolean b) env = (Boolean b, env)
eval (Constant n) env = (Constant n, env)
eval (Symbol s) env = (Symbol s, env)
eval (Combination (Combination [Symbol "lambda", args, body] : values)) env = lambda [args, body] values env
eval (Combination x) env =  combinationEval x env
--}

eval :: [Expr] -> Environment -> [Expr]
eval [] env = []
eval ((Boolean b) : xs) env = Boolean b : eval xs env
eval ((Constant n) : xs) env = Constant n : eval xs env
eval ((Symbol s) : xs) env = Symbol s : eval xs env
eval ((Combination x) : xs) env = (resultExpr result) : (eval xs (resultEnv result))
    where
        result = combinationEval x env
        resultExpr :: (Expr, Environment) -> Expr
        resultExpr (expr, env) = expr 
        resultEnv :: (Expr, Environment) -> Environment
        resultEnv (expr, env) = env

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
    | s == "quote" = quote xs env 1
    | s == "splice" = splice xs env
    | s == "if" = conditional xs env
    -- s == "lambda" = lambda xs env
    | s == "define" = define xs env
    | otherwise = combinationEval ((envLookup env (s)) : xs) env

firstExpr :: [Expr] -> Expr
firstExpr (x:xs) = x

-- >>> runParser program "(add x y)"
-- [([Combination [Symbol "add",Symbol "x",Symbol "y"]],"")]


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
add [Constant e1, Combination e2] env = add [Constant e1,  firstExpr (eval [Combination e2] env)] env
add [Combination e1, Constant e2] env = add [firstExpr (eval [Combination e1] env), Constant e2] env
add [Combination e1, Combination e2] env = add [firstExpr (eval [Combination e1] env), firstExpr (eval [Combination e2] env)] env
add [Symbol x, Symbol y] env = add [envLookup env x, envLookup env y] env

-- add [Combination x] env = eval (Combination x)

sub [Constant e1, Constant e2] env = (Constant (e1 - e2), env)
sub [Combination ((Symbol x):xs)] env = sub xs env
sub [Constant e1, Combination e2] env = sub [Constant e1, firstExpr (eval [Combination e2] env)] env
sub [Combination e1, Constant e2] env = sub [firstExpr (eval [Combination e1] env), Constant e2] env
sub [Combination e1, Combination e2] env = sub [firstExpr (eval [Combination e1] env), firstExpr (eval [Combination e2] env)] env


mult [Constant e1, Constant e2] env = (Constant (e1 * e2), env)
divide [Constant e1, Constant e2] env = (Constant (e1 `div` e2), env)

cons :: [Expr] -> Environment -> (Expr, Environment)
cons [x, Symbol "nil"] env = (Combination [firstExpr (eval [x] env)], env)
cons [x, Combination (Symbol "cons" : xs)] env = cons [firstExpr (eval [x] env), getExpr (cons xs env)] env
cons [x, Combination xs] env = (Combination (firstExpr(eval [x] env) : xs), env)
cons [x, Dot ys y] env = (Dot (firstExpr(eval [x] env) : ys) y, env)
cons [x, y] env = (Dot [firstExpr(eval [x] env)] y, env)
cons _ env = (Symbol "Error", env)


first, second, number :: [Expr] -> Environment -> (Expr, Environment)
first [Combination x] env = a x
    where
        a [Symbol "cons", e1, e2] = (e1, env)
        a [Symbol "quote", Combination y] = first [Combination y] env
        a (e1 : e2) = (e1, env)

second [Combination x] env = a x
    where
        a [Symbol "cons", e1, e2] = (e2, env)
        a [Boolean e1, Boolean e2] = (Boolean e2, env)
        a [Symbol "quote", Combination y] = second [Combination y] env
        a (e1 : e2) = (Combination e2, env)

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
list [Symbol "nil"] env = (Boolean True, env)
list [Symbol e] env = list [envLookup env e] env
list [Combination e] env = list e env
list (x:xs) env = (Boolean True, env)

-- >>> runParser program "(cons 1 (cons 2 (cons 3 nil)))"
-- [([Combination [Symbol "cons",Constant 1,Combination [Symbol "cons",Constant 2,Combination [Symbol "cons",Constant 3,Symbol "nil"]]]],"")]

-- >>> list [Combination [Symbol "cons",Constant 1,Combination [Symbol "cons",Constant 2,Combination [Symbol "cons",Constant 3,Symbol "nil"]]]] (Env [])
-- (Boolean True,Env [])

-- >>> list [Symbol "cons",Constant 3,Symbol "nil"] (Env [])
-- (Boolean True,Env [])

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

quote :: [Expr] -> Environment -> Int -> (Expr, Environment)
quote [Constant x] env n = (Constant x, env)
quote [Symbol s] env 0 =  (Symbol s, env)
quote [Symbol s] env n = (Symbol s, env)
quote [Combination xs] env 0 = (firstExpr (eval xs env), env)
quote [Combination [Symbol "splice", x]] env n = quote [x] env (n - 1) 
quote [Combination [Symbol "quote", x]] env n = quote [x] env (n + 1)
quote [Combination (x:xs)] env n = (Combination xs, env)
-- quote [Combination xs] env = (Combination (map checkSplice xs), env)
--    where
--        checkSplice (Combination [Symbol "splice", x]) = firstExpr(eval [x] env)
--        checkSplice x = x
quote [Dot xs x] env n = (Dot xs x, env)

-- >>> runParser program "'(x '(2 $(add 2 3) $$(add 2 3)))"
-- [([Combination [Symbol "quote",Combination [Symbol "x",Combination [Symbol "quote",Combination [Constant 2,Combination [Symbol "splice",Combination [Symbol "add",Constant 2,Constant 3]],Combination [Symbol "splice",Combination [Symbol "splice",Combination [Symbol "add",Constant 2,Constant 3]]]]]]]],"")]


-- >>> quote [Combination [Symbol "x",Combination [Symbol "quote",Combination [Constant 2,Combination [Symbol "splice",Combination [Symbol "add",Constant 2,Constant 3]],Combination [Symbol "splice",Combination [Symbol "splice",Combination [Symbol "add",Constant 2,Constant 3]]]]]]] (Env []) 1
-- (Combination [Symbol "x",Combination [Symbol "quote",Combination [Constant 2,Combination [Symbol "splice",Combination [Symbol "add",Constant 2,Constant 3]],Combination [Symbol "splice",Combination [Symbol "splice",Combination [Symbol "add",Constant 2,Constant 3]]]]]],Env [])
-- 



splice :: [Expr] -> Environment -> (Expr, Environment)
splice [Constant x] env = (Constant x, env)
splice [Symbol s] env = (Symbol s, env)
-- splice [Combination [Symbol "quote", x]] = eval x
splice [Combination xs] env = (firstExpr (eval [Combination xs] env), env)


conditional :: [Expr] -> Environment -> (Expr, Environment)
conditional [Boolean True, x, y] env = (x, env)
conditional [Boolean False, x, y] env = (y, env)

define :: [Expr] -> Environment -> (Expr, Environment)
define [Symbol var, Constant value] env = envAdd env (var, Constant value)
define [Symbol var, Symbol value] env = envAdd env (var, Symbol value)
define [Symbol var, Combination value] env = envAdd env (var, firstExpr (eval [Combination value] env))


-- >>> runParser program "(define xs \n (cons 1 (cons 2 (cons 3 nil)))) \n (list? xs)"
-- [([Combination [Symbol "define",Symbol "xs",Combination [Symbol "cons",Constant 1,Combination [Symbol "cons",Constant 2,Combination [Symbol "cons",Constant 3,Symbol "nil"]]]],Combination [Symbol "list?",Symbol "xs"]],"")]

-- >>> define [Symbol "xs",Combination [Symbol "cons",Constant 1,Combination [Symbol "cons",Constant 2,Combination [Symbol "cons",Constant 3,Symbol "nil"]]]] (Env [])
-- (Symbol "",Env [("xs",Combination [Constant 1,Constant 2,Constant 3])])

-- >>> second [Combination [Constant 1,Constant 2,Constant 3]] (Env [])
-- (Combination [Constant 2,Constant 3],Env [])

-- >>> map printEval (eval [Combination [Symbol "define",Symbol "xs",Combination [Symbol "cons",Constant 1,Combination [Symbol "cons",Constant 2,Combination [Symbol "cons",Constant 3,Symbol "nil"]]]],Combination [Symbol "list?",Symbol "xs"]] (Env []))
-- ["","#t"]

-- >>> runParser program

-- >>> runParser program "(list? xs)"
-- [([Combination [Symbol "list?",Symbol "xs"]],"")]



assign :: [Expr] -> [Expr] -> [(String, Expr)]
assign [] [] = []
assign (Symbol x:xs) (n:ns) = (x, n) : assign xs ns

-- >>> assign [Symbol "x", Symbol "y"] [Constant 3, Constant 4]
-- [("x",Constant 3),("y",Constant 4)]


lambda :: [Expr] -> [Expr] -> Environment -> (Expr, Environment) -- when you evaluate a lambda/macro, check that the AST node in the args position has the correct form
lambda [Combination arg, body] values (Env es) = (envLookup2 (Env (es ++ assign arg values)) body, Env (es ++ assign arg values))



-- >>> runParser program "((lambda (x y) x) 3 4)"
-- [([Combination [Combination [Symbol "lambda",Combination [Symbol "x",Symbol "y"],Symbol "x"],Constant 3,Constant 4]],"")]
-- 

-- >>> eval (Combination [Combination [Symbol "lambda",Combination [Symbol "x",Symbol "y"],Symbol "x"],Constant 3,Constant 4]) (Env [("k", Constant 5)])
-- (Constant 3,Env [("k",Constant 5),("x",Constant 3),("y",Constant 4)])
-- 

-- >>> eval (Symbol "x",Env [("k",Constant 5),("x",Constant 3),("y",Constant 4)])
-- Couldn't match expected type ‘Expr’
--             with actual type ‘(Expr, Environment)’

-- >>> envLookup (Env [("k",Constant 5),("x",Constant 3),("y",Constant 4)]) "x"
-- Just (Constant 3)


-- >>> lambda [Combination [Symbol "x",Symbol "y"],Symbol "x"] [Constant 3, Constant 4] (Env [("k", Constant 5)])
-- (Symbol "x",Env [("k",Constant 5),("x",Constant 3),("y",Constant 4)])
-- 

-- >>> runParser program "'(1 2 $3)"
-- [([Combination [Symbol "quote",Combination [Constant 1,Constant 2,Combination [Symbol "splice",Constant 3]]]],"")]

-- >>> quote [Combination [Constant 1,Constant 2,Combination [Symbol "splice",Constant 3]]]
-- /home/vscode/github-classroom/Iowa-CS-3820-Fall-2021/project-meta-meta-team/src/Main.hs:(309,9)-(310,40): Non-exhaustive patterns in function multiquote


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

