module LambdaComp.AM.Syntax where

import Control.Monad.State.Strict (State, execState, gets, modify')
import Data.List.NonEmpty         (NonEmpty)
import Data.List.NonEmpty         qualified as NonEmpty
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (mapMaybe)

import LambdaComp.Ident
import Data.Foldable (traverse_)

data Addr where
  AIdent    :: Ident -> Addr
  ALocalEnv :: Int -> Addr
  deriving Show

data Value where
  VaUnit   :: Value
  VaInt    :: Int -> Value
  VaDouble :: Double -> Value
  VaThunk  :: Ident -> [Addr] -> Value
  VaAddr   :: Addr -> Value
  deriving Show

data Inst where
  IScope   :: Inst
  IPush    :: Value -> Inst
  IPop     :: Addr -> Inst
  IAssign  :: Addr -> Value -> Inst
  IJump    :: Value -> Inst
  IReturn  :: Value -> Inst
  IReceive :: Addr -> Inst
  IPrint   :: Value -> Inst
  deriving Show

type Code = [Inst]

data CodeSection
  = ThunkCodeSection
    { thunkCodeSectionName :: Ident
    , thunkCode            :: Code
    , thunkEnvSize         :: Int
    }
  deriving Show

data Item where
  ItUnit   :: Item
  ItInt    :: Int -> Item
  ItDouble :: Double -> Item
  ItThunk  :: Ident -> [Item] -> Item
  deriving Show

type GlobalStack = [Item]
type Envs = NonEmpty (Map Ident Item)
type LocalEnv = [Item]

data EvalState
  = EvalState
    { globalStack :: GlobalStack
    , globalEnvs  :: Envs
    , localEnv    :: LocalEnv
    , returnReg   :: Item
    }

type Eval = State EvalState

topEval :: Code -> Item
topEval c = returnReg $ evalCode (IScope : c) `execState` evalState
  where
    evalState =
      EvalState
      { globalStack = []
      , globalEnvs = Map.empty NonEmpty.:| []
      , localEnv = []
      , returnReg = ItUnit
      }

evalCode :: Code -> Eval ()
evalCode = traverse_ evalInst

evalInst :: Inst -> Eval ()
evalInst = undefined

embodyValue :: Value -> Eval Item
embodyValue VaUnit                 = pure ItUnit
embodyValue (VaInt n)              = pure $ ItInt n
embodyValue (VaDouble d)           = pure $ ItDouble d
embodyValue (VaThunk c globalEnvs) = ItThunk c <$> traverse embodyAddr globalEnvs
embodyValue (VaAddr addr)          = embodyAddr addr

embodyAddr :: Addr -> Eval Item
embodyAddr (AIdent x)    = gets $ head . mapMaybe (Map.lookup x) . NonEmpty.toList . globalEnvs
embodyAddr (ALocalEnv n) = gets $ (!! n) . localEnv

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
    _ ->
      error "Impossible!"

iAssign :: Addr -> Item -> Eval ()
iAssign (AIdent x)    it = modify' (\m -> let h NonEmpty.:| t = globalEnvs m in m{ globalEnvs = Map.insert x it h NonEmpty.:| t })
iAssign (ALocalEnv n) it = modify' (\m -> let (h, t) = splitAt (n - 1) $ localEnv m in m{ localEnv = h <> (it : tail t) })

iReturn :: Item -> Eval ()
iReturn it = modify' (\m -> m{ returnReg = it, globalEnvs = NonEmpty.fromList $ NonEmpty.tail $ globalEnvs m })
