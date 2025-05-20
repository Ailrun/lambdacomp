{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module LambdaComp.AM.Eval
  ( Item(..)

  , topEval
  ) where

import Control.Monad              (unless)
import Control.Monad.Reader       (MonadIO (liftIO), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (StateT, execStateT, gets, modify')
import Data.Bifunctor             (Bifunctor (first, second))
import Data.Either                (lefts)
import Data.List                  ((!?))
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
  deriving stock Show

topEval :: Handle -> [CodeSection] -> IO Item
topEval out cs = returnReg <$> runMachine evalData evalState
  where
    evalData = foldr insertCodeSection (Map.empty, ([], out)) cs
    evalState =
      EvalState
      { codePointer = (Right 0, 0)
      , globalStack = []
      , globalEnvs = Map.empty NonEmpty.:| [Map.empty]
      , localEnv = []
      , callStack = []
      , returnReg = ItInt 0
      }

type SectionPointer = Either Ident Int
type CodePointer = (SectionPointer, Int)
type StackLike a = [a]
type GlobalStack = StackLike Item
type Envs = NonEmpty (Map Ident Item)
type LocalEnv = Vector Item
type CallStackEntry = (CodePointer, LocalEnv)
type CallStack = StackLike CallStackEntry

type EvalData = (Map Ident (Either Value Code), ([Code], Handle))

data EvalState where
  EvalState ::
    { codePointer :: CodePointer
    , globalStack :: GlobalStack
    , globalEnvs  :: Envs
    , localEnv    :: LocalEnv
    , callStack   :: CallStack
    , returnReg   :: Item
    } -> EvalState

type Eval = StateT EvalState (ReaderT EvalData IO)

insertCodeSection :: CodeSection -> EvalData -> EvalData
insertCodeSection TmDefCodeSection {..} = second (first (tmDefCode :))
insertCodeSection ThunkCodeSection {..} = first (Map.insert thunkCodeSectionName (Right thunkCode))

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
  (curr, index) <- gets codePointer
  case curr of
    Right topIdx -> fetchTmDefInst topIdx index
    Left funId   -> fetchThunkCodeInst funId index

fetchTmDefInst :: Int -> Int -> Eval (Maybe Inst)
fetchTmDefInst topIdx index = asks (fst . snd) >>= helper
  where
    helper :: [Code] -> Eval (Maybe Inst)
    helper tops
      | Just code <- tops !? topIdx
      , Just it <- code Vector.!? index = do
          modify' (\m -> m{ codePointer = (Right topIdx, index + 1) })
          pure $ Just it
      | otherwise                       = pure Nothing

fetchThunkCodeInst :: Ident -> Int -> Eval (Maybe Inst)
fetchThunkCodeInst funId index = asks ((Map.! funId) . fst) >>= helper
  where
    helper :: Either a Code -> Eval (Maybe Inst)
    helper (Right code)
      | Just it <- code Vector.!? index = do
          modify' (\m -> m{ codePointer = (Left funId, index + 1) })
          pure $ Just it
    helper _                            = pure Nothing

evalInst :: Inst -> Eval ()
evalInst (IDefine x)             = iDefine x
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
evalInst (IPrintDouble v)        = embodyValue v >>= iPrintDouble
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

iDefine :: Ident -> Eval ()
iDefine x = modify' $ \m ->
  m{ codePointer = (fmap (+ 1) . fst $ codePointer m, 0)
   , globalEnvs = mapLast (Map.insert x $ returnReg m) $ globalEnvs m
   }
  where
    mapLast :: (a -> a) -> NonEmpty a -> NonEmpty a
    mapLast f = NonEmpty.reverse . mapHead f . NonEmpty.reverse

    mapHead :: (a -> a) -> NonEmpty a -> NonEmpty a
    mapHead f (y NonEmpty.:| ys) = f y NonEmpty.:| ys

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
              { codePointer = (Left x, 0)
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
iPrimUnOp PrimIToD = stackIntPop >>= iSetReturn . ItDouble . fromIntegral
iPrimUnOp PrimDNeg = stackDoublePop >>= iSetReturn . ItDouble . negate
iPrimUnOp PrimDToI = stackDoublePop >>= iSetReturn . ItInt . truncate
iPrimUnOp PrimBNot = stackBoolPop >>= iSetReturn . ItBool . not

iPrintInt :: Item -> Eval ()
iPrintInt (ItInt i) = asks (snd . snd) >>= liftIO . flip hPrint i
iPrintInt _         = fail "Invalid PrintInt argument"

iPrintDouble :: Item -> Eval ()
iPrintDouble (ItDouble d) = asks (snd . snd) >>= liftIO . flip hPrint d
iPrintDouble _            = fail "Invalid PrintDouble argument"

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
    ItInt i -> pure i
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
