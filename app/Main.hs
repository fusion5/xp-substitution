{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Natural

import qualified Text.ParserCombinators.Parsec as Parsec
import Control.Monad.State.Strict
import Debug.Trace
import Data.Maybe
import qualified Control.Exception as Exception
import System.IO.Unsafe

import Parser
import Types

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

evalLit (Init   (Lit n)) = Value [] $ ValLit n
evalLit (Eval e (Lit n)) = Value e  $ ValLit n
evalLit x = error $ show x

evalVar :: EState -> EState
evalVar (Eval env (Var x)) = {- Value val -- Eval localEnv (Init val)
  where Just (val, localEnv) = lookupEnv x env -}
  case lookupEnv x env of
    Nothing -> error $ "Undeclared variable: " ++ [x]
    Just (val, localEnv) -> Value localEnv val

evalApp :: EState -> EState
evalApp (Eval env (App (Init f)    param))          = Eval env (App (Eval env f) param)
evalApp (Eval env (App f@(Eval {}) param))          = Eval env (App (step env f) param)
evalApp (Eval env (App v@(Value{}) (Init param)))   = Eval env (App v (Eval env param))
evalApp (Eval env (App v@(Value{}) p@(Eval{})))     = Eval env (App v (step env p))
evalApp (Eval _   (App (Value env (ValClosure x body))
                       (Value _ val@arg)))            = Eval env' body
  where env' = pushEnv x val env

exp0 = ex "(((/x.((/y.(/x.x)) 0)) 1) 2)"
exp1 = ex "(((/x.((/y.(/x.(x y))) (/x.x))) 1) (/x.x))"

evalLam :: EState -> EState
evalLam (Eval env (Lam x body)) = Value env $ ValClosure x body

step :: Env -> EState -> EState
step env v@(Value {}) = v -- error "Value given to step, something went wrong"
step env (Init x)   = Eval env x
step _ x@(Eval _ (Lit {})) = evalLit x
step _ x@(Eval _ (Var {})) = evalVar x
step _ x@(Eval _ (App {})) = evalApp x
step _ x@(Eval _ (Lam {})) = evalLam x

ex = fromJust . parseExpr

stepIO :: Env -> EState -> IO EState
stepIO initEnv es
  = do
    print es
    putStrLn "  =="
    return ret
  where
    ret = step initEnv es

reduce :: EState -> IO EState
reduce es
  = do
    es' <- stepIO [] es
    case es' of
      Value _ _ -> return es'
      _         -> reduce es'

test =
  -- 1
  [ lookupEnv 'a' testEnv
    `equals` Just (ValLit 1, [('b', ValLit 0), ('a', ValLit 0)])
  -- 2
  , lookupEnv 'b' testEnv
    `equals` Just (ValLit 0, [('a', ValLit 0)])
  -- 3
  , lookupEnv 'a' (pushEnv 'a' (ValLit 2) testEnv)
    `equals` Just (ValLit 2, testEnv)
  , step [] (step [] (ex "x"))
    `shouldThrow` "Undeclared variable: x"
  -- TODO: Add variable-capture tests...
  ]

{-
eval :: Expr -> State Env Expr
eval (Lit n) = return (Lit n)
eval (Var x) = do
  env <- get
  case lookupEnv x env of
    Nothing   -> error $ "Variable not found " ++ show x
    Just (expr, env') -> -- withState (const env') $ do
      -- if expr has free variables, and they're not in the environment, then error?
      -- Evaluate expr using the environment it had when it was
      -- declared!
      return $ evalState (eval expr) env'
-- eval (Add e1 e2) = addExpr <$> eval e1 <*> eval e2
eval (App f param) = do
  evalf <- eval f
  evalp <- eval param
  case evalf of
    Lam x body -> do
        env <- get
        return $ evalState (eval body) (pushEnv x evalp env)
    _ -> error "Non-function application!"
eval (Lam x body) = return $ Lam x body
-- eval x = error $ "Eval undefined for " ++ show x

evalExpr :: Expr -> Expr
evalExpr e = evalState (eval e) []

evaluatesTo :: Expr -> Expr -> Bool
evaluatesTo e1 e2 | evalExpr e1 == e2 = True
evaluatesTo e1 e2 | otherwise =
  error $ unlines
    [ "Expression:"
    , show e1
    , "should evaluate to (expected):"
    , show e2
    , "but it evaluated to (got):"
    , show (evalExpr e1)
    ]


-- e1 = (\x.\y.y x) y
captureTest1 = App (Lam 'x' $ Lam 'y' $ App y x) y
captureTest2 = App (Lam 'x' $ Lam 'y' $ App y x) (Lit 1)

test =
  -- 1
  [ lookupEnv 'a' testEnv
    `equals` Just ((Lit 1), [('b', Lit 0), ('a', Lit 0)])
  -- 2
  , lookupEnv 'b' testEnv
    `equals` Just ((Lit 0), [('a', Lit 0)])
  -- 3
  , lookupEnv 'a' (pushEnv 'a' (Lit 2) testEnv)
    `equals` Just ((Lit 2), testEnv)
  -- 4
--  , evalExpr incOne
--    `equals` Lit 2
  -- 5
--  , evalExpr (App addSucc (Lit 5))
--    `equals` Lit 11
  -- 6
  , evalExpr (Var 'y')
    `shouldThrow`
      "Variable not found 'y'"
  -- 7
  , evalExpr (App idFun (Var 'y'))
    `shouldThrow`
      "Variable not found 'y'"
  -- 8
  , evalExpr (App (Lam 'x' $ Lam 'x' $ x) y)
    `shouldThrow`
      "Variable not found 'y'"
  -- 9
  , evalExpr captureTest1
    `shouldThrow`
      "Variable not found 'y'"
    -- `equals`
      -- (Lam 'y' $ App y x)
  -- 10
  , evalExpr (App (Lam 'y' $ App x y) (Lit 1))
    `shouldThrow`
      "Variable not found 'x'"
  -- 11
  , evalExpr (App (Lam 'x' $ x) (Lit 1))
    `equals` (Lit 1)
  -- 12 (\x.x 1) id = 1
  , evalExpr (App (Lam 'x' (App (Var 'x') (Lit 1))) (idFun))
    `equals` (Lit 1)
  -- 13 ()
  ,  captureTest2
      `evaluatesTo`
        (Lam 'y' (App (Var 'y') (Var 'x')))
  -- 14 (\y.(\x.\y.y x) y) 1 -> (\x.\y.y x) 1 -> (\y.y 1)
  , ((Lam 'y' captureTest1) `App` (Lit 1))
      `evaluatesTo`
        (Lam 'y' (App (Var 'y') (Var 'x')))
  -- ((\y.y 1) id) = 1
  , evalExpr (((Lam 'y' captureTest1) `App` (Lit 1)) `App` idFun)
    `equals` (Lit 1)
  ]


testState = do
  modify (pushEnv 'a' (Lit 1))
  env <- get
  when (isNothing (lookupEnv 'a' env)) (error "E1")
  withState (const []) $ do
    env <- get
    when (isJust (lookupEnv 'a' env)) (error "E2")
  env <- get
  when (isNothing (lookupEnv 'a' env)) (error "E3")

    -- lookupEnv =
-}

main = do
  putStrLn "Hello Lambda!"
  -- let s = map show $ zip [1..] test
  -- putStrLn $ unlines s

equals e1 e2 | e1 == e2 = True
equals e1 e2 | otherwise =
  error $ "\nExpecting:\n" ++ show e2 ++ "\nbut got:\n" ++ show e1

shouldThrow :: EState -> String -> Bool
shouldThrow e errMsg = equals e2 errMsg where
  e2 :: String
  e2 = unsafePerformIO $ do
        eith <- Exception.try (Exception.evaluate e)
        case eith of
          Left  (Exception.ErrorCall msg) -> return msg
          Right _ -> return "No exception"

