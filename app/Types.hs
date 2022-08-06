module Types where

import GHC.Natural

type Env = [(Char, Value)]

data Value =
      ValLit     Natural
    | ValClosure
      Env {-
        Needed because otherwise we'd have:
          {[] |- {[('x',1)] |- /(y.x)} 2}
            ==
          {[] |- */y.x* 2} -- results in undefined variable x.
            The environment would then be lost.
      -}
      Char Expr
      deriving (Eq)

data EState
  = Init  Expr
  | Eval  Env Expr
  | Value Value
    deriving (Eq)

data Expr
  = Var Char
  | Lit Natural
  -- | Add Expr Expr
  | Lam Char Expr
  | App EState EState
    deriving (Eq)

surround :: Char -> String -> String
surround c s = showChar c $ showString s [close c]
  where
    close '(' = ')'
    close '{' = '}'
    close '<' = '>'
    close c   = c

{-
instance Show Value where
  show (ValLit n) = show n
  show (ValClosure env x body) = surround '(' $
    show env ++ " |- " ++ surround '(' ("/" ++ [x] ++ "." ++ show body)

instance Show EState where
  show (Init e)    = show e
  -- putStrLn $ "\x1b[32m" ++ "highlight me" ++ "\x1b[0m" ++ " but not me"
  show (Eval env e) =
      surround '{' (show env ++ " |- " ++ show e) -- TODO: we could show the environment too!
  show (Value v) = surround '<' (show v)

instance Show Expr where
  show (Var c)      = [c]
  show (Lit n)      = show n
  -- show (Add e1 e2)  = show e1 ++ " + " ++ show e2
  show (Lam c expr) = surround '(' $ "/" ++ showChar c "" ++ "." ++ show expr
  show (App f x)    = surround '(' $ show f ++ " " ++ show x

-}
