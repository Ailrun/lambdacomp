{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}
module LambdaComp.AM.Eval
  ( Item(..)

  , topEval
  ) where

import Control.Monad              (unless)
import Control.Monad.Reader       (MonadIO (liftIO), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (StateT, execStateT, gets, modify')
import Data.Bifunctor             (Bifunctor (first, second))
import Data.Either                (lefts)
import Data.List                  (foldl')
import Data.List.NonEmpty         (NonEmpty)
import Data.List.NonEmpty         qualified as NonEmpty
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (mapMaybe, maybeToList)
import Data.Tuple                 (swap)
import Data.Vector                (Vector)
import Data.Vector                qualified as Vector
import System.IO                  (Handle, hPrint)

import LambdaComp.AM.Syntax

data Item where
  ItUnit   :: Item
  ItBool   :: !Bool -> Item
  ItInt    :: !Int -> Item
  ItDouble :: !Double -> Item
  ItThunk  :: !Ident -> !(Vector Item) -> Item
  deriving Show

topEval :: Handle -> [CodeSection] -> IO Item
topEval out cs = returnReg <$> runMachine evalData evalState
  where
    evalData = foldl' insertCodeSection (Map.singleton mainName $ Right [ICall "var_u_main"], out) cs
    evalState =
      EvalState
      { codePointer = (mainName, 0)
      , globalStack = []
      , globalEnvs = Map.empty NonEmpty.:| [Map.empty]
      , localEnv = []
      , callStack = []
      , returnReg = ItUnit
      }

type StackLike a = [a]
type GlobalStack = StackLike Item
type Envs = NonEmpty (Map Ident Item)
type LocalEnv = Vector Item
type CallStackEntry = ((Ident, Int), LocalEnv)
type CallStack = StackLike CallStackEntry

type EvalData = (Map Ident (Either Value Code), Handle)

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

insertCodeSection :: EvalData -> CodeSection -> EvalData
insertCodeSection ed TmDefCodeSection {..} = first (Map.insert tmDefCodeSectionName (Left tmDefValue)) ed
insertCodeSection ed ThunkCodeSection {..} = first (Map.insert thunkCodeSectionName (Right thunkCode)) ed

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
  code <- asks ((Map.! funId) . fst)
  helper funId code index
  where
    helper funId (Right code) index
      | Just it <- code Vector.!? index = do
          modify' (\m -> m{ codePointer = (funId, index + 1) })
          pure $ Just it
    helper _     _            _         = pure Nothing

evalInst :: Inst -> Eval ()
evalInst IScope                  = iScope
evalInst (IPush v)               = embodyValue v >>= iPush
evalInst (IPop addr)             = iPop addr
evalInst (IAssign addr v)        = embodyValue v >>= iAssign addr
evalInst (IJump l)               = iJump l
evalInst (ICondJump v l)         = do
  it <- embodyValue v
  iCondJump it l
evalInst (ICall v)               = embodyValue v >>= iCall
evalInst (ISetReturn v)          = embodyValue v >>= iSetReturn
evalInst (IReceive x)            = iReceive x
evalInst (IRecAssign addr x env) = iRecAssign addr x env
evalInst (IPrimBinOp op)         = iPrimBinOp op
evalInst (IPrimUnOp op)          = iPrimUnOp op
evalInst (IPrintInt v)           = embodyValue v >>= iPrintInt
evalInst IExit                   = iExit
evalInst IEndScope               = iEndScope

embodyValue :: Value -> Eval Item
embodyValue VaUnit                 = pure ItUnit
embodyValue (VaBool b)             = pure $ ItBool b
embodyValue (VaInt n)              = pure $ ItInt n
embodyValue (VaDouble d)           = pure $ ItDouble d
embodyValue (VaThunk c globalEnvs) = ItThunk c <$> traverse embodyAddr globalEnvs
embodyValue (VaAddr addr)          = embodyAddr addr

embodyAddr :: Addr -> Eval Item
embodyAddr (AIdent x)    = do
  its <- gets $ mapMaybe (Map.lookup x) . NonEmpty.toList . globalEnvs
  vs <- asks $ lefts . maybeToList . Map.lookup x . fst
  its' <- traverse embodyValue vs
  case its <> its' of
    []     -> fail $ show x <> " is not in scope."
    it : _ -> pure it
embodyAddr (ALocalEnv n) = gets $ (Vector.! n) . localEnv

iScope :: Eval ()
iScope = modify' (\m -> m{ globalEnvs = Map.empty NonEmpty.<| globalEnvs m })

iPush :: Item -> Eval ()
iPush it = modify' (\m -> m{ globalStack = it : globalStack m })

iPop :: Addr -> Eval ()
iPop addr = stackPop >>= iAssign addr

iAssign :: Addr -> Item -> Eval ()
iAssign (AIdent x)    it = modify' (\m -> let h NonEmpty.:| t = globalEnvs m in m{ globalEnvs = Map.insert x it h NonEmpty.:| t })
iAssign (ALocalEnv n) it = modify' (\m -> m{ localEnv = localEnv m Vector.// [(n, it)] })

iJump :: Int -> Eval ()
iJump l =
  modify' $ \m ->
              m{ codePointer = second (+ l) $ codePointer m}

iCondJump :: Item -> Int -> Eval ()
iCondJump (ItBool b) l = unless b $ iJump l
iCondJump _          _ = fail "Impossible!"

iCall :: Item -> Eval ()
iCall (ItThunk x env) =
  modify' $ \m ->
              m
              { codePointer = (x, 0)
              , localEnv = env
              , callStack = (codePointer m, localEnv m) : callStack m
              }
iCall _               = fail "Impossible!"

iSetReturn :: Item -> Eval ()
iSetReturn it = modify' (\m -> m{ returnReg = it })

iEndScope :: Eval ()
iEndScope = modify' (\m -> m{ globalEnvs = NonEmpty.fromList $ NonEmpty.tail $ globalEnvs m })

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

iPrimBinOp :: PrimOp Binary -> Eval ()
iPrimBinOp PrimIAdd = stackIntPop2 >>= iSetReturn . ItInt . uncurry (+) . swap
iPrimBinOp PrimISub = stackIntPop2 >>= iSetReturn . ItInt . uncurry (-) . swap
iPrimBinOp PrimIMul = stackIntPop2 >>= iSetReturn . ItInt . uncurry (*) . swap
iPrimBinOp PrimIDiv = stackIntPop2 >>= iSetReturn . ItInt . uncurry quot . swap
iPrimBinOp PrimIMod = stackIntPop2 >>= iSetReturn . ItInt . uncurry rem . swap
iPrimBinOp PrimIEq  = stackIntPop2 >>= iSetReturn . ItBool . uncurry (==) . swap
iPrimBinOp PrimINEq = stackIntPop2 >>= iSetReturn . ItBool . uncurry (/=) . swap
iPrimBinOp PrimILt  = stackIntPop2 >>= iSetReturn . ItBool . uncurry (<) . swap
iPrimBinOp PrimILe  = stackIntPop2 >>= iSetReturn . ItBool . uncurry (<=) . swap
iPrimBinOp PrimIGt  = stackIntPop2 >>= iSetReturn . ItBool . uncurry (>) . swap
iPrimBinOp PrimIGe  = stackIntPop2 >>= iSetReturn . ItBool . uncurry (>=) . swap
iPrimBinOp PrimDAdd = stackDoublePop2 >>= iSetReturn . ItDouble . uncurry (+) . swap
iPrimBinOp PrimDSub = stackDoublePop2 >>= iSetReturn . ItDouble . uncurry (-) . swap
iPrimBinOp PrimDMul = stackDoublePop2 >>= iSetReturn . ItDouble . uncurry (*) . swap
iPrimBinOp PrimDDiv = stackDoublePop2 >>= iSetReturn . ItDouble . uncurry (/) . swap
iPrimBinOp PrimDEq  = stackDoublePop2 >>= iSetReturn . ItBool . uncurry (==) . swap
iPrimBinOp PrimDNEq = stackDoublePop2 >>= iSetReturn . ItBool . uncurry (/=) . swap
iPrimBinOp PrimDLt  = stackDoublePop2 >>= iSetReturn . ItBool . uncurry (<) . swap
iPrimBinOp PrimDLe  = stackDoublePop2 >>= iSetReturn . ItBool . uncurry (<=) . swap
iPrimBinOp PrimDGt  = stackDoublePop2 >>= iSetReturn . ItBool . uncurry (>) . swap
iPrimBinOp PrimDGe  = stackDoublePop2 >>= iSetReturn . ItBool . uncurry (>=) . swap
iPrimBinOp PrimBAnd = stackBoolPop2 >>= iSetReturn . ItBool . uncurry (&&) . swap
iPrimBinOp PrimBOr  = stackBoolPop2 >>= iSetReturn . ItBool . uncurry (||) . swap

iPrimUnOp :: PrimOp Unary -> Eval ()
iPrimUnOp PrimINeg = stackIntPop >>= iSetReturn . ItInt . negate
iPrimUnOp PrimDNeg = stackDoublePop >>= iSetReturn . ItDouble . negate
iPrimUnOp PrimBNot = stackBoolPop >>= iSetReturn . ItBool . not

iPrintInt :: Item -> Eval ()
iPrintInt (ItInt n) = asks snd >>= liftIO . flip hPrint n
iPrintInt _         = fail "Invalid PrintInt argument"

iExit :: Eval ()
iExit = do
  prevCallStack <- gets callStack
  case prevCallStack of
    []                                  -> fail "Invalid Exit"
    (codePointer, localEnv) : callStack -> modify' (\m -> m{ codePointer, localEnv, callStack })

stackIntPop2 :: Eval (Int, Int)
stackIntPop2 = liftA2 (,) stackIntPop stackIntPop

stackIntPop :: Eval Int
stackIntPop = do
  it <- stackPop
  case it of
    ItInt n -> pure n
    _       -> fail "Stack pop gives a non-int item"

stackDoublePop2 :: Eval (Double, Double)
stackDoublePop2 = liftA2 (,) stackDoublePop stackDoublePop

stackDoublePop :: Eval Double
stackDoublePop = do
  it <- stackPop
  case it of
    ItDouble d -> pure d
    _          -> fail "Stack pop gives a non-double item"

stackBoolPop2 :: Eval (Bool, Bool)
stackBoolPop2 = liftA2 (,) stackBoolPop stackBoolPop

stackBoolPop :: Eval Bool
stackBoolPop = do
  it <- stackPop
  case it of
    ItBool b -> pure b
    _        -> fail "Stack pop gives a non-bool item"

stackPop :: Eval Item
stackPop = do
  stack <- gets globalStack
  case stack of
    h : t -> do
      modify' (\m -> m{ globalStack = t })
      pure h
    []    -> fail "Invalid stack pop"

mainName :: Ident
mainName = "main"
