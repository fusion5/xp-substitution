{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Natural

import qualified Text.ParserCombinators.Parsec as Parsec
import Control.Monad.State.Strict
import Debug.Trace
import Data.Maybe
import qualified Control.Exception as Exception
import System.IO.Unsafe
import Data.List
import Control.Exception

import Parser
import Types
import ColorPretty

testEnv =
    pushEnv 'a' (ValLit 1)
  $ pushEnv 'b' (ValLit 0)
  $ pushEnv 'a' (ValLit 0) []

one    = Lit 1
-- incFun = Lam 'x' (Add x one)
-- incOne = App incFun one
x      = Var 'x'
y      = Var 'y'
idFun  :: EState
idFun  = fromJust $ parseExpr "(/x.x)"

-- addSucc = Lam 'x' (Add x (App incFun x))


pushEnv :: Char -> Value -> Env -> Env
pushEnv c e env = (c, e):env -- env ++ (c, e):[]

popEnv :: Env -> Env
popEnv = tail

lookupRest :: (Eq a) => a -> [(a,b)] -> Maybe (b, [(a, b)])
lookupRest _key [] =  Nothing
lookupRest  key ((x,y):xys)
    | key == x  =  Just (y, xys)
    | otherwise =  lookupRest key xys

lookupEnv :: Char -> Env -> Maybe (Value, Env)
lookupEnv = lookupRest

addExpr :: Expr -> Expr -> Expr
addExpr (Lit n1) (Lit n2) = Lit (n1 + n2)
addExpr t1 t2 = error $
  "Addition error, trying to add " ++ show t1 ++ " to " ++ show t2

-- TODO: Evaluate to a value which is either
-- a literal, or
-- a closure?

evalLit (Init   (Lit n)) = Value $ ValLit n
evalLit (Eval e (Lit n)) = Value $ ValLit n
evalLit x = error $ show x

evalVar :: EState -> EState
evalVar (Eval env (Var x)) = {- Value val -- Eval localEnv (Init val)
  where Just (val, localEnv) = lookupEnv x env -}
  case lookupEnv x env of
    Nothing -> error $ "Undeclared variable: " ++ [x]
    Just (val, localEnv) -> Value val

mergeEnvsFaulty :: Env -> Env -> Env -> Env
mergeEnvsFaulty commonSuffix env1 env2 =
    assert (commonSuffix `isSuffixOf` env1) $
      assert (commonSuffix `isSuffixOf` env2) $
        prefix1 ++ prefix2 ++ commonSuffix
  where
    n = length commonSuffix
    nenv1 = length env1
    nenv2 = length env2
    prefix1 = take (n - nenv1) env1
    prefix2 = take (n - nenv2) env2

evalApp
  :: Env    -- Environment
  -> EState -- Function
  -> EState -- Parameter
  -> EState -- Evaluation progress
evalApp env (Init f)                        param
  = Eval env (App (Eval env f) param)
evalApp env f@(Eval {})                     param
  = Eval env (App (step env f) param)
evalApp env v@(Value{})                     (Init param)
  = Eval env (App v (Eval env param))
evalApp env v@(Value{})                     p@(Eval{})
  = Eval env (App v (step env p))
evalApp env (Value (ValClosure fEnv x e))   (Value (ValLit n))
  = Eval env' e
  where env' = pushEnv x (ValLit n) fEnv
evalApp env (Value (ValClosure fEnv x1 e1)) (Value param@(ValClosure paramEnv x2 e2))
  = Eval env' e1
  -- This discards env, but this shouldn't be a problem because
  -- env is a suffix of both fEnv and paramEnv (I think!).
  where env' = pushEnv x1 param fEnv

exp0 = ex "(((/x.((/y.(/x.x)) 0)) 1) 2)"
exp1 = ex "(((/x.((/y.(/x.(x y))) (/x.x))) 1) (/x.x))"

evalLam :: EState -> EState
evalLam (Eval env (Lam x body)) = Value $ ValClosure env x body
-- evalLam (Eval env (Lam x body)) = Eval env (Lam x body)

step :: Env -> EState -> EState
step env v@(Value {}) = v -- error "Value given to step, something went wrong"
step env (Init x)   = Eval env x
step _ x@(Eval _ (Lit {})) = evalLit x
step _ x@(Eval _ (Var {})) = evalVar x
step _ (Eval env (App f arg)) = evalApp env f arg
step _ x@(Eval _ (Lam {})) = evalLam x

ex = fromJust . parseExpr

stepIO :: Env -> EState -> IO EState
stepIO initEnv es
  = do
    putStrLn $ pp es
    putStrLn "  =="
    return ret
  where
    ret = step initEnv es

reduce :: EState -> EState
reduce es =
  case step [] es of
    Value es' -> Value es'
    x         -> reduce x

reduceIO :: EState -> IO EState
reduceIO es
  = do
    es' <- stepIO [] es
    case es' of
      Value _ -> return es'
      _       -> reduceIO es'

redExIO = unsafePerformIO . reduceIO . ex

redEx = reduce . ex

test :: [Bool]
test =
  [ equals "1"
      (lookupEnv 'a' testEnv)
      (Just (ValLit 1, [('b', ValLit 0), ('a', ValLit 0)]))
  , equals "2"
      (lookupEnv 'b' testEnv)
      (Just (ValLit 0, [('a', ValLit 0)]))
  , equals "3"
      (lookupEnv 'a' (pushEnv 'a' (ValLit 2) testEnv))
      (Just (ValLit 2, testEnv))
  , step [] (step [] (ex "x"))
      `shouldThrow` "Undeclared variable: x"
  -- TODO: Add variable-capture tests...
  , equals "4"
      (redEx "(((/x.(/x.x)) 1) 2)") (Value (ValLit 2))
  , redEx "(((/x.(/x.y)) 1) 2)"
      `shouldThrow` "Undeclared variable: y"
  , equals "5" (redEx "(((/x.(/y.x)) 1) 2)") (Value (ValLit 1))
  , equals "6" (redEx "(((/y.(/x.x)) 1) 2)") (Value (ValLit 2))
  , redEx "(((/y.(/y.x)) 1) 2)"
      `shouldThrow` "Undeclared variable: x"
  , equals "7"
      (redEx "(((/x.((/x.x) x)) (/x.x)) 2)")
      (Value (ValLit 2))

  -- Combinations of 3 parameters, all combinations modulo
  -- alpha equivalence:
  , "((((/x.(/x.(/x.x))) 0) 1) 2)" `reducesTo` Value (ValLit 2)
  , "((((/x.(/x.(/y.x))) 0) 1) 2)" `reducesTo` Value (ValLit 1)
  , "((((/x.(/x.(/y.y))) 0) 1) 2)" `reducesTo` Value (ValLit 2)
  , "((((/x.(/y.(/x.x))) 0) 1) 2)" `reducesTo` Value (ValLit 2)
  , "((((/x.(/y.(/x.y))) 0) 1) 2)" `reducesTo` Value (ValLit 1)
  , "((((/x.(/y.(/y.x))) 0) 1) 2)" `reducesTo` Value (ValLit 0)
  , "((((/x.(/y.(/y.y))) 0) 1) 2)" `reducesTo` Value (ValLit 2)
  , "(((/x.((/x.(/x.x)) 0)) 1) 2)" `reducesTo` Value (ValLit 2)
  , "(((/x.((/x.(/y.x)) 0)) 1) 2)" `reducesTo` Value (ValLit 0)
  , "(((/x.((/x.(/y.y)) 0)) 1) 2)" `reducesTo` Value (ValLit 2)
  , "(((/x.((/y.(/x.x)) 0)) 1) 2)" `reducesTo` Value (ValLit 2)
  , "(((/x.((/y.(/x.y)) 0)) 1) 2)" `reducesTo` Value (ValLit 0)
  , "(((/x.((/y.(/y.x)) 0)) 1) 2)" `reducesTo` Value (ValLit 1)
  , "(((/x.((/y.(/y.y)) 0)) 1) 2)" `reducesTo` Value (ValLit 2)

  , "(((/x.(/y.x)) 1) ((/x.(/y.x)) 2))" `reducesTo` Value (ValLit 1)
  , "(((/x.(/y.(y x))) 1) ((/x.(/y.x)) 2))" `reducesTo` Value (ValLit 2)
  -- , "(((/x.(/y.x)) 1) ((/x.(/y.x)) 2))" `reducesToIO` Value (ValLit 1)
  , "(((/x.(/y.(((/y.(/x.(x y))) 3) ((/y.(/x.y)) 4)))) 1) 2)" `reducesTo` Value (ValLit 4)

  -- , equals "((/z.(((/x.(/y.(y x))) 1) z)) ((/x.(/y.x)) 2))"
  --    (redEx "((/z.(((/x.(/y.(y x))) 1) z)) ((/x.(/y.x)) 2))") (Value (ValLit 3))
  ]

reducesToIO str = equals str (redExIO str)
reducesTo str = equals str (redEx str)

testO = "((/z.(((/x.(/y.z)) 1) ((/x.(/y.x)) 2))) 3)"
testJ = "((/x.x) ((/x.(/y.x)) 1))"
testK = "((/x.(/y.x)) 1)"

main = do
  putStrLn "Hello!"
  let s = zipWith (curry show) [1..] test
  putStrLn $ unlines s

equals _ e1 e2 | e1 == e2 = True
equals s e1 e2 =
  error $ "\nExpecting:\n" ++ show e2 ++ "\nbut got:\n" ++ show e1 ++ " when testing " ++ s

shouldThrow :: EState -> String -> Bool
shouldThrow e = equals "shouldThrow" e2
  where
    e2 :: String
    e2 = unsafePerformIO $ do
          eith <- Exception.try (Exception.evaluate e)
          case eith of
            Left  (Exception.ErrorCall msg) -> return msg
            Right _ -> return "No exception"

