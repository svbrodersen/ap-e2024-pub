module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
 where
  runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
  runEval' _ _ (Pure x) = ([], pure x)
  runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
  runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
  runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
  runEval' r s (Free (PrintOp p m)) =
    let (ps, res) = runEval' r s m
     in (p : ps, res)
  runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
  runEval' r s (Free (TryCatchOp m1 m2)) =
    case runEval' r s m1 of
      (_, Left _) -> runEval' r s m2
      (s', Right x) -> (s', Right x)
  runEval' r s (Free (KvGetOp v f)) =
    case lookup v s of
      Nothing -> ([], Left $ "Invalid Key: " ++ show v)
      (Just v1) -> runEval' r s (f v1)
  runEval' r s (Free (KvPutOp v1 v2 k)) =
    case lookup v1 s of
      Nothing -> runEval' r ((v1, v2) : s) k
      Just _ -> runEval' r ((v1, v2) : s') k
   where
    -- We remove the item, where the key is with filter.
    s' = filter (\(z, _) -> z /= v1) s
  runEval' r s (Free (TransactionOp k m)) =
    case runEval' r s k of
      (ps, Left _) ->
        let (ps', res) = runEval' r s m
         in (ps ++ ps', res)
      (ps, Right _) ->
        let (ps', res) =
              runEval'
                r
                s
                ( do
                    _ <- k
                    m
                )
         in (ps ++ ps', res)
