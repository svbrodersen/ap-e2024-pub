{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  threadDelay,
 )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.Foldable (for_)
import Data.List (partition)
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
type Error = String

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
  | MsgWorkerAdd WorkerName (ReplyChan (Either Error String))
  | MsgDoesWorkerExist WorkerName (ReplyChan Bool)
  | MsgWorkerJobDone JobId JobDoneReason
  | MsgWorkerShutDown WorkerName

-- | A handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
newtype Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)]
  , spcJobsRunning :: [(JobId, (WorkerName, Seconds))]
  , spcJobsDone :: [(JobId, JobDoneReason)]
  , spcJobCounter :: JobId
  , spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))]
  , spcIdleWorkers :: [(WorkerName, Worker)]
  , spcBusyWorkers :: [(WorkerName, Worker)]
  , spcWorkerJobs :: [(WorkerName, JobId)]
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
schedule = do
  state <- get
  case (spcJobsPending state, spcIdleWorkers state) of
    ((jobId, job) : pendingJobs', (workerName, worker) : idleWorkers') -> do
      now <- io getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $
        state
          { spcJobsPending = pendingJobs'
          , spcJobsRunning = (jobId, (workerName, deadline)) : spcJobsRunning state
          , spcIdleWorkers = idleWorkers'
          , spcBusyWorkers = (workerName, worker) : spcBusyWorkers state
          , spcWorkerJobs = (workerName, jobId) : spcWorkerJobs state
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
          put $
            state
              { spcJobsRunning = spcJobsRunning'
              , spcJobsDone = spcJobsDone'
              , spcWaiting = notWaitingForJob
              , spcBusyWorkers = spcBusyWorkers'
              , spcWorkerJobs = spcWorkerJobs'
              , spcIdleWorkers = (workerName, worker') : spcIdleWorkers state
              }
        Nothing ->
          put $
            state
              { spcJobsRunning = spcJobsRunning'
              , spcJobsDone = spcJobsDone'
              , spcWaiting = notWaitingForJob
              , spcWorkerJobs = spcWorkerJobs'
              , spcBusyWorkers = spcBusyWorkers'
              }
    Nothing ->
      put $
        state
          { spcJobsRunning = spcJobsRunning'
          , spcJobsDone = spcJobsDone'
          , spcWaiting = notWaitingForJob
          }

workerExists :: WorkerName -> SPCState -> Bool
workerExists workerName state =
  workerName
    `elem` ( map fst (spcIdleWorkers state)
              ++ map
                fst
                (spcBusyWorkers state)
           )

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  let timedOutJobs =
        [ (jobId, workerName)
        | (jobId, (workerName, deadline)) <- spcJobsRunning state
        , now >= deadline
        ]
  for_ timedOutJobs $ \(jobId, workerName) -> do
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
      put $
        state
          { spcJobsPending = (newJobId, job) : spcJobsPending state
          , spcJobCounter = newJobId
          }
      io $ reply rsvp newJobId
    MsgJobStatus jobId rsvp -> do
      state <- get
      let status
            | any ((== jobId) . fst) (spcJobsPending state) = JobPending
            | any ((== jobId) . fst) (spcJobsRunning state) = JobRunning
            -- If lookup returns nothing this is JobUnknown. Otherwise it is
            -- passed to JobDone
            | otherwise = maybe JobUnknown JobDone $ lookup jobId $ spcJobsDone state
      io $ reply rsvp status
    MsgJobWait jobId rsvp -> do
      state <- get
      case lookup jobId (spcJobsDone state) of
        Just reason -> io $ reply rsvp $ Just reason
        Nothing ->
          if any ((== jobId) . fst) (spcJobsPending state)
            || any ((== jobId) . fst) (spcJobsRunning state)
            then put $ state{spcWaiting = (jobId, rsvp) : spcWaiting state}
            else io $ reply rsvp Nothing
    MsgJobCancel jobId -> do
      state <- get
      if any ((== jobId) . fst) (spcJobsPending state)
        then do
          put $ state{spcJobsPending = removeAssoc jobId (spcJobsPending state)}
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
    MsgWorkerAdd workerName rsvp -> do
      state <- get
      let exists = workerExists workerName state
      if exists
        then io $ reply rsvp $ Left $ "Worker with name '" ++ workerName ++ "' already exists."
        else do
          workerServer <- io $ spawn $ \c' -> workerMain c' c workerName
          let worker = Worker workerServer
          put $ state{spcIdleWorkers = (workerName, worker) : spcIdleWorkers state}
          io $ reply rsvp $ Right $ "Successfully added worker '" ++ workerName ++ "'."
    MsgWorkerJobDone jobId reason -> do
      jobDone jobId reason
    MsgWorkerShutDown workerName -> do
      state <- get
      case lookup workerName $ spcBusyWorkers state of
        Nothing ->
          case lookup workerName $ spcIdleWorkers state of
            Nothing ->
              pure ()
            Just (Worker workerServer) -> do
              workerCancel workerName workerServer state
        Just (Worker workerServer) ->
          workerCancel workerName workerServer state

      {- WorkerCancel might update the state. If worker is running a job. -}
      newState <- get
      let spcIdleWorkers' = removeAssoc workerName (spcIdleWorkers newState)
      let spcWorkerJobs' = removeAssoc workerName (spcWorkerJobs newState)
      put $
        newState
          { spcIdleWorkers = spcIdleWorkers'
          , spcWorkerJobs = spcWorkerJobs'
          }
     where
      workerCancel n' s' state' = do
        {- Update state to remove from Busy workers. This makes it such that
         - jobDone does not try and update the worker to be idle. If Worker is
         - running a job while it is shutDown, then the job cancels.-}
        let spcBusyWorkers' = removeAssoc workerName (spcBusyWorkers state')
        put $
          state'
            { spcBusyWorkers = spcBusyWorkers'
            }
        case lookup n' $ spcWorkerJobs state' of
          Nothing -> do
            io $ sendTo s' WorkerMsgShutDown
          Just jobId -> do
            jobDone jobId DoneCancelled
            io $ sendTo s' WorkerMsgShutDown
    MsgDoesWorkerExist workerName rsvp -> do
      state <- get
      io $ reply rsvp $ workerExists workerName state
    MsgTick -> pure ()

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
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

{- | Add a new worker with this name. Fails with 'Left' if a worker
with that name already exists.
-}
workerAdd :: SPC -> WorkerName -> IO (Either Error String)
workerAdd (SPC c) workerName = do
  requestReply c $ MsgWorkerAdd workerName

{- | Shut down a running worker. No effect if the worker is already
terminated.
-}
workerStop :: SPC -> WorkerName -> IO ()
workerStop (SPC c) workerName =
  sendTo c $ MsgWorkerShutDown workerName

workerMain :: Chan WorkerMsg -> Chan SPCMsg -> WorkerName -> IO ()
workerMain c' s' n' =
  innerHandler c' s' n' Nothing
 where
  innerHandler c spcChan workerName mtid =
    do
      msg <- receive c
      case msg of
        WorkerMsgStartJob jobId job -> do
          {- Make sure we only have a single thread started -}
          case mtid of
            Nothing -> do
              t <- forkIO $ do
                let doJob = do
                      jobAction job
                      send c $ WorkerMsgJobDone jobId Done
                    onException :: SomeException -> IO ()
                    onException _ =
                      send c $ WorkerMsgJobDone jobId DoneCrashed
                doJob `catch` onException
              innerHandler c spcChan workerName (Just t)
            Just tid ->
              innerHandler c spcChan workerName (Just tid)
        WorkerMsgJobDone jobId reason -> do
          send spcChan $ MsgWorkerJobDone jobId reason
          innerHandler c spcChan workerName Nothing
        WorkerMsgCancelJob jobId -> do
          for_ mtid killThread
          send spcChan $ MsgWorkerJobDone jobId DoneCancelled
          innerHandler c spcChan workerName mtid
        WorkerMsgJobTimedOut jobId -> do
          for_ mtid killThread
          send spcChan $ MsgWorkerJobDone jobId DoneTimeout
          innerHandler c spcChan workerName mtid
        WorkerMsgShutDown -> do
          for_ mtid killThread
