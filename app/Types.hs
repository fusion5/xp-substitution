module Types where

import GHC.Natural

type Env = [(Char, Value)]

data Value =
      ValLit Natural
    | ValClosure Char Expr
      deriving (Eq)

data EState
  = Init  Expr
  | Eval  Env Expr
  | Value Env Value
    deriving (Eq)

data Expr
  = Var Char
  | Lit Natural
  -- | Add Expr Expr
  | Lam Char Expr
  | App EState EState
    deriving (Eq)

surround :: Char -> String -> String
surround c s = c : s ++ [close c]
  where
    close '(' = ')'
    close '{' = '}'
    close c   = c

instance Show Value where
  show (ValLit n) = show n
  show (ValClosure x body) = "/" ++ [x] ++ "." ++ show body

instance Show EState where
  show (Init e)    = show e
  show (Eval _ e)  = surround '{' (show e) -- TODO: we could show the environment too!
  show (Value env v) = surround '*' (show env ++ " |- " ++ show v)

instance Show Expr where
  show (Var c)      = [c]
  show (Lit n)      = show n
  -- show (Add e1 e2)  = show e1 ++ " + " ++ show e2
  show (Lam c expr) = "/(" ++ showChar c "" ++ "." ++ show expr ++ ")"
  show (App f x)    = show f ++ " " ++ show x


