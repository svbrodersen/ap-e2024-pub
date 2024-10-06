module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import GHC.RTS.Flags (DoTrace (TraceNone))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)])
    , --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5))
    , --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True))
    , --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)])
    , --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ())
    , --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!")
    , --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
    , let goodPut = evalKvPut (ValInt 0) (ValInt 1)
          badPut = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
          get0 = KvGet (CstInt 0)
       in testGroup
            "TransactionOp Test"
            [ testCase "goodput" $
                runEval
                  ( transaction goodPut
                      >> eval get0
                  )
                  @?= ([], Right (ValInt 1))
            , testCase "badPut" $
                runEval
                  ( transaction badPut
                      >> eval get0
                  )
                  @?= ([], Left "Invalid Key: ValInt 0")
            , testCase "printOp effects" $
                runEval
                  ( transaction (evalPrint "weee" >> failure "oh shit")
                  )
                  @?= (["weee"], Right ())
            , testCase "nested 1" $
                runEval
                  ( transaction (transaction goodPut) >> eval get0
                  )
                  @?= ([], Right (ValInt 1))
            , testCase "nested 2" $
                runEval
                  (transaction (transaction badPut) >> eval get0)
                  @?= ([], Left "Invalid Key: ValInt 0")
            ]
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ())
    , -- NOTE: This test will give a runtime error unless you replace the
      -- version of `eval` in `APL.Eval` with a complete version that supports
      -- `Print`-expressions. Uncomment at your own risk.
      testCase "print 2" $ do
        (out, res) <-
          captureIO [] $
            evalIO' $
              Print "This is also 1" $
                Print "This is 1" $
                  CstInt 1
        (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    , testCase "Missing key test" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free $
                KvGetOp (ValInt 0) $
                  \val -> pure val
        res @?= Right (ValInt 1)
    , let goodPut = evalKvPut (ValInt 0) (ValInt 1)
          get0 = KvGet (CstInt 0)
       in testGroup
            "TransactionOp IO Test"
            [ testCase "goodput" $
                do
                  val <-
                    runEvalIO
                      ( transaction goodPut
                          >> eval get0
                      )
                  val @?= Right (ValInt 1)
            , {- No badput test due to missing key test before -}
              testCase "printOp effects" $
                do
                  res <-
                    captureIO ["weee"] $
                      runEvalIO
                        ( transaction (evalPrint "weee" >> failure "oh shit")
                        )
                  res @?= (["weee"], Right ())
            , testCase "nested 1" $
                do
                  res <-
                    runEvalIO
                      ( transaction (transaction goodPut) >> eval get0
                      )
                  res @?= Right (ValInt 1)
            ]
    ]
