{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}
module LambdaComp.AM.Eval where

import Control.Monad.Reader       (MonadIO (liftIO), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (StateT, execStateT, gets, modify')
import Data.List                  (foldl')
import Data.List.NonEmpty         (NonEmpty)
import Data.List.NonEmpty         qualified as NonEmpty
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (mapMaybe)
import Data.Vector                (Vector)
import Data.Vector                qualified as Vector

import LambdaComp.AM.Syntax

data Item where
  ItUnit   :: Item
  ItInt    :: !Int -> Item
  ItDouble :: !Double -> Item
  ItThunk  :: !Ident -> !(Vector Item) -> Item
  deriving Show

type StackLike a = [a]
type GlobalStack = StackLike Item
type Envs = NonEmpty (Map Ident Item)
type LocalEnv = Vector Item
type CallStackEntry = ((Ident, Int), LocalEnv)
type CallStack = StackLike CallStackEntry

type EvalData = Map Ident Code

data EvalState
  = EvalState
    { codePointer :: (Ident, Int)
    , globalStack :: GlobalStack
    , globalEnvs  :: Envs
    , localEnv    :: LocalEnv
    , callStack   :: CallStack
    , returnReg   :: Item
    }

type Eval = StateT EvalState (ReaderT EvalData IO)

topEval :: [CodeSection] -> IO Item
topEval cs = returnReg <$> runMachine evalData evalState
  where
    evalData = foldl' insertCodeSection Map.empty cs
    evalState =
      EvalState
      { codePointer = (mainName, 0)
      , globalStack = []
      , globalEnvs = Map.empty NonEmpty.:| [Map.empty]
      , localEnv = []
      , callStack = []
      , returnReg = ItUnit
      }

insertCodeSection :: EvalData -> CodeSection -> EvalData
insertCodeSection ed MainCodeSection {..}  = Map.insert mainName mainCode ed
insertCodeSection ed ThunkCodeSection {..} = Map.insert thunkCodeSectionName thunkCode ed

runMachine :: EvalData -> EvalState -> IO EvalState
runMachine evalData evalState = go `execStateT` evalState `runReaderT` evalData
  where
    go :: Eval ()
    go = do
      mayInst <- fetchInst
      case mayInst of
        Just inst -> evalInst inst >> go
        Nothing   -> pure ()

fetchInst :: Eval (Maybe Inst)
fetchInst = do
  (funId, index) <- gets codePointer
  code <- asks (Map.! funId)
  helper funId code index
  where
    helper funId code index
      | Just it <- code Vector.!? index = do
          modify' (\m -> m{ codePointer = (funId, index + 1) })
          pure $ Just it
      | otherwise                       = pure Nothing

evalInst :: Inst -> Eval ()
evalInst IScope                  = iScope
evalInst (IPush v)               = embodyValue v >>= iPush
evalInst (IPop addr)             = iPop addr
evalInst (IAssign addr v)        = embodyValue v >>= iAssign addr
evalInst (IJump v)               = embodyValue v >>= iJump
evalInst (IReturn v)             = embodyValue v >>= iReturn
evalInst (IReceive x)            = iReceive x
evalInst (IRecAssign addr x env) = iRecAssign addr x env
evalInst (IPrintInt v)           = embodyValue v >>= iPrintInt
evalInst IExit                   = iExit

embodyValue :: Value -> Eval Item
embodyValue VaUnit                 = pure ItUnit
embodyValue (VaInt n)              = pure $ ItInt n
embodyValue (VaDouble d)           = pure $ ItDouble d
embodyValue (VaThunk c globalEnvs) = ItThunk c <$> traverse embodyAddr globalEnvs
embodyValue (VaAddr addr)          = embodyAddr addr

embodyAddr :: Addr -> Eval Item
embodyAddr (AIdent x)    = do
  its <- gets $ mapMaybe (Map.lookup x) . NonEmpty.toList . globalEnvs
  case its of
    []     -> error $ show x <> " is not in scope!!"
    it : _ -> pure it
embodyAddr (ALocalEnv n) = gets $ (Vector.! n) . localEnv

iScope :: Eval ()
iScope = modify' (\m -> m{ globalEnvs = Map.empty NonEmpty.<| globalEnvs m })

iPush :: Item -> Eval ()
iPush it = modify' (\m -> m{ globalStack = it : globalStack m })

iPop :: Addr -> Eval ()
iPop addr = do
  stack <- gets globalStack
  case stack of
    h : t -> do
      modify' (\m -> m{ globalStack = t })
      iAssign addr h
    []    -> error "Impossible!"

iAssign :: Addr -> Item -> Eval ()
iAssign (AIdent x)    it = modify' (\m -> let h NonEmpty.:| t = globalEnvs m in m{ globalEnvs = Map.insert x it h NonEmpty.:| t })
iAssign (ALocalEnv n) it = modify' (\m -> m{ localEnv = localEnv m Vector.// [(n, it)] })

iJump :: Item -> Eval ()
iJump (ItThunk x env) =
  modify' $ \m ->
              m
              { codePointer = (x, 0)
              , localEnv = env
              , callStack = (codePointer m, localEnv m) : callStack m
              }
iJump _               = error "Impossible!"

iReturn :: Item -> Eval ()
iReturn it = modify' (\m -> m{ returnReg = it, globalEnvs = NonEmpty.fromList $ NonEmpty.tail $ globalEnvs m })

iReceive :: Addr -> Eval ()
iReceive addr = do
  it <- gets returnReg
  iAssign addr it

iRecAssign :: Addr -> Ident -> Vector Addr -> Eval ()
iRecAssign addr x env = do
  rec
    thunk <- ItThunk x <$> traverse (embodyEnv thunk) env
  iAssign addr thunk
  where
    embodyEnv :: Item -> Addr -> Eval Item
    embodyEnv thunk addr'
      | addr == addr' = pure thunk
      | otherwise     = embodyAddr addr'

iPrintInt :: Item -> Eval ()
iPrintInt (ItInt n) = liftIO $ print n
iPrintInt _         = error "Impossible!"

iExit :: Eval ()
iExit = do
  prevCallStack <- gets callStack
  case prevCallStack of
    []                                  -> error "Impossible!"
    (codePointer, localEnv) : callStack -> modify' (\m -> m{ codePointer, localEnv, callStack })

mainName :: Ident
mainName = "main"
