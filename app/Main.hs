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
import Tests

import Parser
import Types
import ColorPretty
import Internal


one    = Lit 1
-- incFun = Lam 'x' (Add x one)
-- incOne = App incFun one
x      = Var 'x'
y      = Var 'y'
idFun  :: EState
idFun  = fromJust $ parseExpr "(/x.x)"

-- addSucc = Lam 'x' (Add x (App incFun x))



main = do
  putStrLn "Hello!"
  -- let s = zipWith (curry pp) [1..] test
  -- putStrLn $ unlines s


