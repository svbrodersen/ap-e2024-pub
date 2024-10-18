module SPC (
  -- * SPC startup
  SPC,
  startSPC,
  pingSPC,
  Job (..),
  JobId,
)
where

import Control.Concurrent (
  ThreadId,
  forkIO,
  killThread,
  threadDelay,
 )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
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

data SPCState = SPCState
  { spcPingCounter :: Int
  , queue :: [(JobId, Job)]
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state ->
    do
      (x', state') <- m state
      let SPCM y = f x'
      y state'

get :: SPCM SPCState
get = SPCM $ \state ->
  pure (state, state)

put :: SPCState -> SPCM ()
put state = SPCM $ \_ ->
  pure ((), state)

io :: IO a -> SPCM a
io x = SPCM $ \state ->
  do
    x' <- x
    pure (x', state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM m) = fst <$> m state

-- Messages sent to SPC.
data SPCMsg = MsgPing (ReplyChan Int)

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

data Job = Job
  { jobAction :: IO ()
  , jobMaxSeconds :: Int
  }

newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

jobAdd :: SPC -> Job -> IO JobId
jobAdd = undefined

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c =
  do
    msg <- io $ receive c
    case msg of
      MsgPing c' ->
        do
          state <- get
          io $ reply c' $ spcPingCounter state
          put $ state{spcPingCounter = succ $ spcPingCounter state}

startSPC :: IO SPC
startSPC = do
  server <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC server
 where
  initial_state = SPCState{spcPingCounter = 0}

pingSPC :: SPC -> IO Int
pingSPC (SPC s) =
  requestReply s MsgPing
