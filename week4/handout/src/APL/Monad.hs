module APL.Monad (
  envEmpty,
  envExtend,
  envLookup,
  stateInitial,
  askEnv,
  modifyEffects,
  localEnv,
  getState,
  putState,
  modifyState,
  evalKvGet,
  evalKvPut,
  evalPrint,
  failure,
  catch,
  EvalM,
  Val (..),
  EvalOp (..),
  Free (..),
  Error,
  Env,
  State,
)
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ fmap h g
   where
    h x = x >>= f

data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a

instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ \env -> f (k env)
  fmap f (StateGetOp k) = StateGetOp $ \state -> f (k state)
  fmap f (StatePutOp s m) = StatePutOp s (f m)

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = error "TODO"

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv = error "TODO"

getState :: EvalM State
getState = error "TODO"

putState :: State -> EvalM ()
putState = error "TODO"

modifyState :: (State -> State) -> EvalM ()
modifyState = error "TODO"

evalPrint :: String -> EvalM ()
evalPrint = error "TODO"

failure :: String -> EvalM a
failure = error "TODO"

catch :: EvalM a -> EvalM a -> EvalM a
catch = error "To be completed in assignment 4."

evalKvGet :: Val -> EvalM Val
evalKvGet = error "To be completed in assignment 4."

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut = error "To be completed in assignment 4."
