module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123
        , parserTest " 123" $ CstInt 123
        , parserTest "123 " $ CstInt 123
        , parserTestFail "123f"
        , parserTest "true" $ CstBool True
        , parserTest "false" $ CstBool False
        ]
    , testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y")
        , parserTest "x-y" $ Sub (Var "x") (Var "y")
        , parserTest "x*y" $ Mul (Var "x") (Var "y")
        , parserTest "x/y" $ Div (Var "x") (Var "y")
        ]
    , testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z")
        , parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z")
        , parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z"))
        , parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z")
        , parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ]
    , testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z")
        , parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z")
        , parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z")
        , parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ]
    , testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2
        , parserTest " 2" $ CstInt 2
        ]
    , testGroup
        "Function application"
        [ parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z") -- From examples (Shows left associativity)
        , parserTest "a b c d" $ Apply (Apply (Apply (Var "a") (Var "b")) (Var "c")) (Var "d")
        , parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")) -- From examples (Shows that it binds tighter than other operators)
        , parserTest "a(b(c d))" $ Apply (Var "a") (Apply (Var "b") (Apply (Var "c") (Var "d")))
        ]
    , testGroup
        "Equality and power operations"
        [ testGroup
            "Power test"
            [ parserTestFail "x***y"
            , parserTest "x*y**z**k" $
                Mul
                  (Var "x")
                  ( Pow
                      (Var "y")
                      ( Pow
                          (Var "z")
                          (Var "k")
                      )
                  ) -- Shows right associativity
            , parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")) -- Shows higher precedence than multiplication (and other operators)
            , parserTest "x**y*z" $ Mul (Pow (Var "x") (Var "y")) (Var "z")
            , parserTest "x**y" $ Pow (Var "x") (Var "y")
            ]
        , testGroup
            "Equality test"
            [ parserTest "x+y==y+x" $
                Eql
                  (Add (Var "x") (Var "y"))
                  ( Add
                      ( Var
                          "y"
                      )
                      (Var "x")
                  )
            , parserTest "x==y" $ Eql (Var "x") (Var "y")
            , parserTest "x==y==z" $ Eql (Eql (Var "x") (Var "y")) (Var "z")
            , parserTest "x+y*z/f**i==c" $
                Eql
                  ( Add
                      (Var "x")
                      ( Div
                          ( Mul
                              (Var "y")
                              (Var "z")
                          )
                          (Pow (Var "f") (Var "i"))
                      )
                  )
                  (Var "c") -- Operator priority
            , parserTest "x**y**z" $ Pow (Var "x") (Pow (Var "y") (Var "z")) -- Right associative testing 
            ]
        ]
    , testGroup
        "Printing, putting and getting"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y") -- From examples
        , parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y") -- From examples
        , parserTest "print \"foo\" x" $ Print "foo" (Var "x") -- From examples
        , parserTest "getx" $ Var "getx" -- From examples
        , parserTest "putx" $ Var "putx"
        , parserTest "printx" $ Var "printx"
        ]
    , testGroup
        "Lambdas, let-binding and try-catch"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z") -- From examples
        , parserTestFail "let true = y in z" -- From examples
        , parserTestFail "x let v = 2 in v" -- From examples
        , parserTest "\\x -> y" $ Lambda "x" (Var "y")
        , parserTest "try x catch y" $ TryCatch (Var "x") (Var "y")
        ]
    ]
