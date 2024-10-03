module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> a
runEval = runEval' envEmpty stateInitial
 where
  runEval' :: Env -> State -> EvalM a -> a
  runEval' _ _ (Pure x) = x
  runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
  runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
  runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
