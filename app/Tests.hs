module Tests where

import qualified Control.Exception as Exception
import Types
import Internal
import ColorPretty
import System.IO.Unsafe

testEnv =
    pushEnv 'a' (ValLit 1)
  $ pushEnv 'b' (ValLit 0)
  $ pushEnv 'a' (ValLit 0) []

equals _ e1 e2 | e1 == e2 = True
equals s e1 e2 =
  error $ "\nExpecting:\n" ++ pp e2 ++ "\nbut got:\n" ++ pp e1 ++ " when testing " ++ s

shouldThrow :: EState -> String -> Bool
shouldThrow e expected = got == expected
  where
    got :: String
    got = unsafePerformIO $ do
          eith <- Exception.try (Exception.evaluate e)
          case eith of
            Left  (Exception.ErrorCall msg) -> return msg
            Right _ -> return "No exception"

reducesToIO str = equals str (redExIO str)
reducesTo str = equals str (redEx str)
inNSteps n str = equals str (stepEx n str)
inNStepsIO n str = equals str (unsafePerformIO $ stepExIO str n (ex str))

testO = "((/z.(((/x.(/y.z)) 1) ((/x.(/y.x)) 2))) 3)"
testJ = "((/x.x) ((/x.(/y.x)) 1))"
testK = "((/x.(/y.x)) 1)"

test :: [Bool]
test =
  [ inNSteps 8
      "(((/x.(/y.x)) 1) 2)"
      (Eval
        []
        (App
          (Value
            (ValClosure
              [('x', ValLit 1)]
              'y'
              (Var 'x'))
          )
          (Init (Lit 2))
        )
      )
  , equals "1"
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
