module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
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
    , --
      testCase "TryCatchOp m1 fails" $ 
        runEval (Free (TryCatchOp (failure "Oh no!") (pure (ValInt 1))))
            @?= ([], Right (ValInt 1))
    , --
      testCase "TryCatchOp m1 succeeds" $
        runEval (Free (TryCatchOp (pure (ValInt 5)) (pure (ValInt 1))))
          @?= ([], Right (ValInt 5))
    , --
      testCase "KvPutOp and KvGetOp test" $ 
        runEval (Free (KvPutOp (ValInt 0) (ValInt 1) (Free (KvGetOp (ValInt 0) (\val -> pure val)))))
          @?= ([], Right (ValInt 1))
    , -- 
      testCase "KvPutOp replace key" $ 
        runEval (Free (KvPutOp (ValInt 0) (ValInt 1) (Free (KvPutOp (ValInt 0) (ValInt 2) (Free (KvGetOp (ValInt 0) (\val -> pure val)))))))
          @?= ([], Right (ValInt 2))
    , -- 
      testCase "KvGetOp missing key" $ 
        runEval (Free (KvGetOp (ValInt 0) (\val -> pure val)))
          @?= ([], Left "Invalid Key: ValInt 0")
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
      -- testCase "print 2" $ do
      --    (out, res) <-
      --      captureIO [] $
      --        evalIO' $
      --          Print "This is also 1" $
      --            Print "This is 1" $
      --              CstInt 1
      --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
      testCase "Missing key test" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free $
                KvGetOp (ValInt 0) $
                  \val -> pure val
        res @?= Right (ValInt 1)
    , --
      testCase "TryCatchOp m1 fails" $ do
        res <- 
          runEvalIO $
            Free (TryCatchOp (failure "Oh no!") (pure (ValInt 1)))
        res @?= Right (ValInt 1)
    , --
      testCase "TryCatchOp m1 succeeds" $ do
        res <- 
          runEvalIO $
            Free (TryCatchOp (pure (ValInt 5)) (pure (ValInt 1)))
        res @?= Right (ValInt 5)
    , -- 
      testCase "TryCatchOp failing comparison" $ do
        let badEal = Eql (CstInt 0) (CstBool True)
            div0 = Div (CstInt 1) (CstInt 0)
        res <- 
          runEvalIO $
            Free (TryCatchOp (eval badEal) (eval div0))
        res @?= Left "Division by zero"
    , -- 
      testCase "KvPutOp and KvGetOp test" $ do
        res <- 
          runEvalIO $
            Free (KvPutOp (ValInt 0) (ValInt 1) (Free (KvGetOp (ValInt 0) (\val -> pure val))))
        res @?= Right (ValInt 1)
    , --
      testCase "KvPutOp replace key" $ do
        res <- 
          runEvalIO $
            Free (KvPutOp (ValInt 0) (ValInt 1) (Free (KvPutOp (ValInt 0) (ValInt 2) (Free (KvGetOp (ValInt 0) (\val -> pure val)))))
            )
        res @?= Right (ValInt 2)

    ]
