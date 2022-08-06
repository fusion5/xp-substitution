module Internal where

import ColorPretty
import GHC.Natural
import System.IO.Unsafe
import Types
import Parser
import Data.Maybe
import Control.Exception
import Data.List

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
  "Addition error, trying to add " ++ pp t1 ++ " to " ++ pp t2

-- TODO: Evaluate to a value which is either
-- a literal, or
-- a closure?

evalLit (Init   (Lit n)) = Value $ ValLit n
evalLit (Eval e (Lit n)) = Value $ ValLit n
evalLit x = error $ pp x

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
step _ v@(Value {}) = error "Value given to step, something went wrong"
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

stepEx n = go n . ex
  where
    go 0 e = e
    go n e = go (n-1) (step [] e)

stepExIO :: String -> Natural -> EState -> IO EState
stepExIO _ 0 e = return e
stepExIO str n e
  = do
    es' <- stepIO [] e
    stepExIO str (n-1) es'

