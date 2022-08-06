{-# LANGUAGE FlexibleInstances #-}
module ColorPretty where

import Types
import Control.Monad.State
import Debug.Trace
import Data.List


type ColorStack = [String]

ppSurround :: Char -> State ColorStack String -> State ColorStack String
ppSurround c inner = do
    s <- inner
    return $ showChar c $ showString s [close c]
  where
    close '(' = ')'
    close '{' = '}'
    close '<' = '>'
    close c   = c

ppColor :: String -> State ColorStack String -> State ColorStack String
ppColor c body = do
  xs <- get   -- peek the current color that we want to restore
  modify (c:) -- push the new color
  sub <- body -- render the body
  modify tail -- pop the color
  let term = case xs of []    -> "0"
                        old:_ -> old
  return $ showString "\x1b[" $ showString c $ showChar 'm'
    $ showString sub $ showString "\x1b[" $ showString term "m"

ppEnv :: Env -> State ColorStack String
ppEnv env = do
    xs <- mapM ppEntry env
    return $ showChar '[' $ showString (intercalate ", " xs) "]"
  where ppEntry (x, val) = showChar x . showString "->" <$> ppValue val

ppValue :: Value -> State ColorStack String
ppValue (ValLit n) = return (show n)
ppValue (ValClosure env x body) = ppSurround '(' $ do
  pe <- ppColor "32" (ppEnv env)
  pb <- ppExpr body
  return $ showString pe $ showString " |- "
    $ surround '(' (showChar '/' $ showChar x $ showChar '.' pb)

ppExpr :: Expr -> State ColorStack String
ppExpr (Var c) = return [c]
ppExpr (Lit n) = return $ show n
ppExpr (Lam c expr) = ppSurround '(' $ do
  pe <- ppExpr expr
  return $ showChar '/' $ showChar c $ showChar '.' pe
ppExpr (App f x) = ppSurround '(' $ do
  pf <- ppEState f
  px <- ppEState x
  return $ showString pf $ showChar ' ' px


ppEState :: EState -> State ColorStack String
ppEState (Init e) = ppExpr e
ppEState (Eval env e) = ppSurround '{' $ do
  penv <- ppColor "32" (ppEnv env)
  pe   <- ppExpr e
  return $ showString penv $ showString " |- " pe
ppEState (Value v) = ppSurround '<' (ppValue v)


class Pretty a where
  pp :: a -> String

instance Pretty EState where
  -- pp :: EState -> String
  pp e = evalState (ppEState e) []

instance Pretty Expr where
  -- pp :: Expr -> String
  pp e = evalState (ppExpr e) []

instance Pretty Env where
  -- pp :: Expr -> String
  pp e = evalState (ppEnv e) []

instance Pretty Value where
  -- pp :: Expr -> String
  pp e = evalState (ppValue e) []

instance Pretty (Value, Env) where
  -- pp :: Expr -> String
  pp (val, env) =
    showString (pp env) $ showString " |- " (pp val)

instance Pretty a => Pretty (Maybe a) where
  pp (Just x) = pp x
  pp Nothing  = "Nothing"
