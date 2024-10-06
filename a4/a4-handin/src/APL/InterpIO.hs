module APL.InterpIO (runEvalIO) where

import APL.InterpPure (runEval)
import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
 where
  runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
  runEvalIO' _ _ (Pure x) = pure $ pure x
  runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
  runEvalIO' r db (Free (StateGetOp f)) =
    do
      k <- readDB db
      case k of
        (Left err) -> pure $ Left err
        (Right s) -> runEvalIO' r db (f s)
  runEvalIO' r db (Free (StatePutOp s m)) =
    do
      _ <- writeDB db s
      runEvalIO' r db m
  runEvalIO' r db (Free (PrintOp p m)) = do
    putStrLn p
    runEvalIO' r db m
  runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
  runEvalIO' r db (Free (TryCatchOp m1 m2)) =
    do
      k1 <- runEvalIO' r db m1
      case k1 of
        (Left _) -> runEvalIO' r db m2
        (Right x) -> pure $ Right x
  runEvalIO' r db (Free (KvGetOp v1 f)) =
    do
      k <- readDB db
      case k of
        (Left err) -> pure $ Left err
        (Right s) ->
          case lookup v1 s of
            Nothing -> retry_key
            (Just v2) -> runEvalIO' r db (f v2)
         where
          retry_key =
            do
              s' <- prompt $ "Invalid key: " ++ show v1 ++ ". Enter a replacement: "
              case readVal s' of
                Nothing -> pure $ Left $ "Invalid value input: " ++ s'
                (Just v2) -> runEvalIO' r db (f v2)
  runEvalIO' r db (Free (KvPutOp v1 v2 k)) =
    do
      file <- readDB db
      case file of
        (Left err) -> pure $ Left err
        (Right dbState) ->
          case lookup v1 dbState of
            Nothing ->
              do
                writeDB db ((v1, v2) : dbState)
                runEvalIO' r db k
            (Just _) ->
              do
                writeDB db dbState'
                runEvalIO' r db k
         where
          dbState' = filter (\(z, _) -> z /= v1) dbState
  runEvalIO' r db (Free (TransactionOp k m)) =
    do
      withTempDB tempFunc
      runEvalIO' r db m
   where
    tempFunc db' =
      do
        res <- runEvalIO' r db' k
        case res of
          (Right _) ->
            do
              copyDB db' db
              pure ()
          (Left _) ->
            pure ()
