module SPC
  ( -- * SPC startup
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

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void, when)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
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
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
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

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

data WorkerMsg
  = WorkerMsgStartJob JobId Job
  | WorkerMsgJobDone JobId JobDoneReason
  | WorkerMsgCancelJob JobId
  | WorkerMsgJobTimedOut JobId
  | WorkerMsgShutDown

data SPCMsg
  = MsgJobAdd Job (ReplyChan JobId)
  | MsgJobCancel JobId
  | MsgJobStatus JobId (ReplyChan JobStatus)
  | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | MsgTick
  | MsgWorkerAdd WorkerName Worker
  | MsgDoesWorkerExist WorkerName (ReplyChan Bool)
  | MsgWorkerJobDone WorkerName JobId JobDoneReason
  | MsgWorkerShutDown WorkerName

-- | A handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
newtype Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, (WorkerName, Seconds))],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcIdleWorkers :: [(WorkerName, Worker)],
    spcBusyWorkers :: [(WorkerName, Worker)],
    spcWorkerJobs :: [(WorkerName, JobId)]
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
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

-- Scheduler functions

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobsPending state, spcIdleWorkers state) of
    ((jobId, job) : pendingJobs', (workerName, worker) : idleWorkers') -> do
      now <- io getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $ state
        { spcJobsPending = pendingJobs',
          spcJobsRunning = (jobId, (workerName, deadline)) : spcJobsRunning state,
          spcIdleWorkers = idleWorkers',
          spcBusyWorkers = (workerName, worker) : spcBusyWorkers state,
          spcWorkerJobs = (workerName, jobId) : spcWorkerJobs state
        }
      let Worker workerServer = worker
      io $ sendTo workerServer $ WorkerMsgStartJob jobId job
      schedule
    _ -> pure ()

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobId reason = do
  state <- get
  let (waitingForJob, notWaitingForJob) = partition ((== jobId) . fst) (spcWaiting state)
  forM_ waitingForJob $ \(_, rsvp) ->
    io $ reply rsvp $ Just reason
  let spcJobsRunning' = removeAssoc jobId (spcJobsRunning state)
  let spcJobsDone' = (jobId, reason) : spcJobsDone state
  case lookup jobId (spcJobsRunning state) of
    Just (workerName, _) -> do
      let spcBusyWorkers' = removeAssoc workerName (spcBusyWorkers state)
      let spcWorkerJobs' = removeAssoc workerName (spcWorkerJobs state)
      let worker = lookup workerName (spcBusyWorkers state)
      case worker of
        Just worker' -> do
          put $ state
            { spcJobsRunning = spcJobsRunning',
              spcJobsDone = spcJobsDone',
              spcWaiting = notWaitingForJob,
              spcBusyWorkers = spcBusyWorkers',
              spcWorkerJobs = spcWorkerJobs',
              spcIdleWorkers = (workerName, worker') : spcIdleWorkers state
            }
        Nothing -> put $ state
            { spcJobsRunning = spcJobsRunning',
              spcJobsDone = spcJobsDone',
              spcWaiting = notWaitingForJob,
              spcWorkerJobs = spcWorkerJobs',
              spcBusyWorkers = spcBusyWorkers'
            }
    Nothing -> put $ state
        { spcJobsRunning = spcJobsRunning',
          spcJobsDone = spcJobsDone',
          spcWaiting = notWaitingForJob
        }

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  let timedOutJobs = [ (jobId, workerName)
                     | (jobId, (workerName, deadline)) <- spcJobsRunning state
                     , now >= deadline
                     ]
  forM_ timedOutJobs $ \(jobId, workerName) -> do
    case lookup workerName (spcBusyWorkers state) of
      Just worker -> do
        let Worker workerServer = worker
        io $ sendTo workerServer $ WorkerMsgJobTimedOut jobId
        jobDone jobId DoneTimeout
      Nothing -> pure ()

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobId = spcJobCounter state
      let newJobId = JobId (jobId + 1)
      put $ state
        { spcJobsPending = (newJobId, job) : spcJobsPending state,
          spcJobCounter = newJobId
        }
      io $ reply rsvp newJobId
      schedule
    MsgJobStatus jobId rsvp -> do
      state <- get
      let status =
            if any ((== jobId) . fst) (spcJobsPending state)
              then JobPending
              else if any ((== jobId) . fst) (spcJobsRunning state)
                then JobRunning
                else case lookup jobId (spcJobsDone state) of
                  Just reason -> JobDone reason
                  Nothing -> JobUnknown
      io $ reply rsvp status
    MsgJobWait jobId rsvp -> do
      state <- get
      case lookup jobId (spcJobsDone state) of
        Just reason -> io $ reply rsvp $ Just reason
        Nothing ->
          if any ((== jobId) . fst) (spcJobsPending state)
             || any ((== jobId) . fst) (spcJobsRunning state)
          then put $ state { spcWaiting = (jobId, rsvp) : spcWaiting state }
          else io $ reply rsvp Nothing
    MsgJobCancel jobId -> do
      state <- get
      if any ((== jobId) . fst) (spcJobsPending state)
        then do
          put $ state { spcJobsPending = removeAssoc jobId (spcJobsPending state) }
          jobDone jobId DoneCancelled
        else case lookup jobId (spcJobsRunning state) of
          Just (workerName, _) -> do
            case lookup workerName (spcBusyWorkers state) of
              Just worker -> do
                let Worker workerServer = worker
                io $ sendTo workerServer $ WorkerMsgCancelJob jobId
                jobDone jobId DoneCancelled
              Nothing -> pure ()
          Nothing -> pure ()
    MsgWorkerAdd workerName worker -> do
      state <- get
      put $ state { spcIdleWorkers = (workerName, worker) : spcIdleWorkers state }
      schedule
    MsgWorkerJobDone workerName jobId reason -> do
      jobDone jobId reason
      schedule
    MsgWorkerShutDown workerName -> do
      state <- get
      let spcIdleWorkers' = removeAssoc workerName (spcIdleWorkers state)
      let spcBusyWorkers' = removeAssoc workerName (spcBusyWorkers state)
      let spcWorkerJobs' = removeAssoc workerName (spcWorkerJobs state)
      put $ state
        { spcIdleWorkers = spcIdleWorkers',
          spcBusyWorkers = spcBusyWorkers',
          spcWorkerJobs = spcWorkerJobs'
        }
    MsgDoesWorkerExist workerName rsvp -> do
      state <- get
      let exists = workerName `elem` (map fst (spcIdleWorkers state) ++ map fst (spcBusyWorkers state))
      io $ reply rsvp exists
    MsgTick -> pure ()

startSPC :: IO SPC
startSPC = do
  let initial_state = SPCState
        { spcJobsPending = []
        , spcJobsRunning = []
        , spcJobsDone = []
        , spcJobCounter = JobId 0
        , spcWaiting = []
        , spcIdleWorkers = []
        , spcBusyWorkers = []
        , spcWorkerJobs = []
        }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobId =
  requestReply c $ MsgJobStatus jobId

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobId =
  requestReply c $ MsgJobWait jobId

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobId =
  sendTo c $ MsgJobCancel jobId

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) workerName = do
  exists <- requestReply c $ \rsvp -> MsgDoesWorkerExist workerName rsvp
  if exists
    then return $ Left $ "Worker with name '" ++ workerName ++ "' already exists."
    else do
      workerServer <- spawn $ \c' -> workerMain c' (SPC c) workerName Nothing
      let worker = Worker workerServer
      sendTo c $ MsgWorkerAdd workerName worker
      return $ Right worker

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop (Worker workerServer) =
  sendTo workerServer WorkerMsgShutDown

workerMain :: Chan WorkerMsg -> SPC -> WorkerName -> Maybe ThreadId -> IO ()
workerMain c (SPC spcServer) workerName mThreadId = do
  msg <- receive c
  case msg of
    WorkerMsgStartJob jobId job -> do
      tId <- forkIO $ do
        let doJob = do
              jobAction job
              send c $ WorkerMsgJobDone jobId Done
            onException :: SomeException -> IO ()
            onException _ =
              send c $ WorkerMsgJobDone jobId DoneCrashed
        doJob `catch` onException
      workerMain c (SPC spcServer) workerName (Just tId)
    WorkerMsgJobDone jobId reason -> do
      sendTo spcServer $ MsgWorkerJobDone workerName jobId reason
      workerMain c (SPC spcServer) workerName Nothing
    WorkerMsgCancelJob jobId -> do
      case mThreadId of
        Just tId -> killThread tId
        Nothing -> pure ()
      sendTo spcServer $ MsgWorkerJobDone workerName jobId DoneCancelled
      workerMain c (SPC spcServer) workerName Nothing
    WorkerMsgJobTimedOut jobId -> do
      case mThreadId of
        Just tId -> killThread tId
        Nothing -> pure ()
      sendTo spcServer $ MsgWorkerJobDone workerName jobId DoneTimeout
      workerMain c (SPC spcServer) workerName Nothing
    WorkerMsgShutDown -> do
      case mThreadId of
        Just tId -> killThread tId
        Nothing -> pure ()
      sendTo spcServer $ MsgWorkerShutDown workerName
    _ -> workerMain c (SPC spcServer) workerName mThreadId