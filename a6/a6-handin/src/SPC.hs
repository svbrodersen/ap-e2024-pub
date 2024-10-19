module SPC (
  -- * SPC startup
  SPC,
  startSPC,

  -- * Job functions
  Job (..),
  JobId,
  JobStatus (..),
  JobDoneReason (..),
  jobAdd,
  jobStatus,
  jobWait,
  jobCancel,

  -- * Worker functions
  WorkerName,
  workerAdd,
  workerStop,
)
where

import Control.Concurrent (
  forkIO,
  killThread,
  newChan,
  threadDelay,
 )
import Control.Exception (Exception, throwIO)
import Control.Monad (ap, forever, liftM, void)
import Data.Data (Typeable)
import Data.IORef
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

{- | Retrieve Unix time using a monotonic clock. You cannot use this
to measure the actual world time, but you can use it to measure
elapsed time.
-}
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { jobAction :: IO ()
  -- ^ The IO action that comprises the actual action of the job.
  , jobMaxSeconds :: Int
  -- ^ The maximum allowed runtime of the job, counting from when
  -- the job begins executing (not when it is enqueued).
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

{- | A worker decides its own human-readable name. This is useful for
debugging.
-}
type WorkerName = String

{- | Messages sent to workers. These are sent both by SPC and by
processes spawned by the workes.
-}
data WorkerMsg
  = MsgJob JobId Job

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Some time has passed.
    MsgTick
  | -- | Add worker to Idle, Reply with Either string worker
    MsgWorkerAdd WorkerName (Server SPCMsg) (ReplyChan String)
  | -- | Worker has finished given job.
    MsgWorkerDone WorkerName JobId JobDoneReason

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)]
  , spcJobsRunning :: [(JobId, Job)]
  , spcJobsDone :: [(JobId, JobDoneReason)]
  , spcJobCounter :: JobId
  , spcWorkersIdle :: [(WorkerName, Worker)]
  , spcWorkersRunning :: [(WorkerName, Worker)]
  , spcWorkerCounter :: Int
  }

{- | The monad in which the main SPC thread runs. This is a state
monad with support for IO.
-}
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule =
  do
    state <- get
    case spcWorkersIdle state of
      [] ->
        {- No available workers. Stop -}
        pure ()
      ((name, Worker w) : xs) ->
        case spcJobsPending state of
          [] ->
            {- No Job to be run -}
            pure ()
          ((jobid, j) : js) -> do
            {- Remove Job from Pending to Running -}
            put $
              state
                { spcJobsPending = js
                , spcJobsRunning = (jobid, j) : spcJobsRunning state
                }

            {- Remove Worker from Idle to Running -}
            put $
              state
                { spcWorkersIdle = xs
                , spcWorkersRunning = (name, Worker w) : spcWorkersRunning state
                }
            {- Send job to worker -}
            io $ sendTo w $ MsgJob jobid j

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason =
  do
    state <- get
    case lookup jobid $ spcJobsDone state of
      Just _ ->
        -- We already know this job is done.
        pure ()
      Nothing -> do
        put $
          state
            { spcJobsDone = (jobid, reason) : spcJobsDone state
            , spcJobsPending = removeAssoc jobid $ spcJobsPending state
            }

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle name worker =
  do
    state <- get
    case lookup name $ spcWorkersIdle state of
      Just _ ->
        {- We already know it is idle -}
        pure ()
      Nothing ->
        do
          removeWorkerRunning state
          addWorkerIdle state
 where
  removeWorkerRunning state =
    put $
      state
        { spcWorkersRunning = removeAssoc name $ spcWorkersRunning state
        }
  addWorkerIdle state =
    put $
      state
        { spcWorkersIdle =
            (name, worker) : spcWorkersIdle state
        }

workerIsGone :: WorkerName -> SPCM ()
workerIsGone = undefined

checkTimeouts :: SPCM ()
checkTimeouts = pure () -- change in Task 4

workerExists :: WorkerName -> SPCM Bool
workerExists name =
  do
    state <- get
    case lookup name (spcWorkersRunning state) of
      Nothing ->
        case lookup name (spcWorkersIdle state) of
          Nothing ->
            pure False
          Just _ -> pure True
      Just _ -> pure True

workerHandleMsg :: Chan WorkerMsg -> WorkerName -> Server SPCMsg -> IO ()
workerHandleMsg c n s =
  do
    msg <- receive c
    case msg of
      MsgJob jobid j ->
        do
          _ <- jobAction j
          sendTo s $ MsgWorkerDone n jobid Done
          pure ()

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobCancel _ -> undefined
    MsgJobWait _ _ -> undefined
    MsgTick -> undefined
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state
          , spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state
                             , lookup jobid $ spcJobsRunning state
                             , lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgWorkerAdd name server rsvp -> do
      {- Make sure worker name is neither idle or running, before replying. -}
      state <- get
      case lookup name $ spcWorkersIdle state of
        Nothing ->
          case lookup name $ spcWorkersRunning state of
            Nothing ->
              do
                s <- io $ spawn $ \c' -> forever $ workerHandleMsg c' name server
                let worker = trace "Defined worker" Worker s
                let counter = spcWorkerCounter state
                put $
                  state
                    { spcWorkerCounter = succ counter
                    , spcWorkersIdle =
                        (name, worker) : spcWorkersIdle state
                    }
                io $ reply rsvp $ "Added worker: " ++ name
            Just _ ->
              workerInUse
        Just _ ->
          workerInUse
     where
      workerInUse =
        io $ reply rsvp "Worker name already in use"
    MsgWorkerDone name jobid reason -> do
      state <- get
      jobDone jobid reason
      case lookup name $ spcWorkersRunning state of
        Nothing ->
          -- Only if the worker is running can it be marked done
          trace "Nothing in lookup" $ pure ()
        Just worker ->
          trace "Worker Idle" workerIsIdle name worker

-- MsgWorkerDone name -> do
--   state <- get
--   case lookup name ()

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0
          , spcJobsPending = []
          , spcJobsRunning = []
          , spcJobsDone = []
          , spcWorkersRunning = []
          , spcWorkersIdle = []
          , spcWorkerCounter = 0
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
 where
  timer c _ = forever $ do
    threadDelay 1000000 -- 1 second
    sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

{- | Add a new worker with this name. Fails with 'Left' if a worker
with that name already exists.
-}
workerAdd :: SPC -> WorkerName -> IO String
workerAdd (SPC s) n =
  requestReply s $ MsgWorkerAdd n s

{- | Shut down a running worker. No effect if the worker is already
terminated.
-}
workerStop :: Worker -> IO ()
workerStop = undefined
