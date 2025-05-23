module LambdaComp.CBPV.ArityAnalysis
  ( runArityAnalysis
  ) where

import Control.Applicative            (liftA3)
import Control.Monad.FreshName        (FreshName, MonadFreshName, freshNamesOf, runFreshName)
import Control.Monad.Reader           (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict     (StateT, evalStateT, gets, modify')
import Control.Monad.Trans            (MonadTrans (lift))
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.CPS       (MonadWriter (tell), WriterT, censor, listens)
import Data.Bifunctor                 (Bifunctor (bimap, first, second))
import Data.Coerce                    (coerce)
import Data.List                      (foldl')
import Data.Map                       (Map)
import Data.Map                       qualified as Map
import Data.String                    (IsString (fromString))

import LambdaComp.CBPV.Syntax
import LambdaComp.CBPV.TypeCheck

type ArityAnalysisDown = ReaderT ArgLen (WriterT Arity FreshName)
type ArityAnalysisUp = ReaderT Context FreshName

-- Note [Where should one eta-expand using arity?]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- - Top-level definitions
-- - Recursive expressions
--
-- possible candidate
-- - if branches
--   Currently, we do not expand those as we do not have
--   enough type information there without re-doing type check.

-- Note [The order of arity analysis]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- - First, we obtain correct arity for all top-level definitions,
--   While so, we flatten and eta-expand things according to the arity.
--   For the specific description about eta-expanded definitions,
--   see note [Where should one eta-expand using arity?].
--   (arityAnalysisInTermDown)
--   Note that this step requires type informations, so the input program
--   should be well-typed.
--
-- - Then, we re-eta-expand things using type information.
--   (arityAnalysisInTermUp)

runArityAnalysis :: Program -> Program
runArityAnalysis p
  | Right ctx <- runProgramInfer p = runFreshName $ do
      (downP, ctx') <- runWriterT . (`evalStateT` (ctx, Arity Map.empty)) . fmap reverse . mapM goDown $ reverse p
      mapM goUp downP `runArityAnalysisUp` ctx'
  | otherwise                      = error "Ill-typed program!"
  where
    goUp :: Top -> ArityAnalysisUp Top
    goUp = arityAnalysisInTopUp

    goDown :: Top -> StateT (Context, Arity) (WriterT Context FreshName) Top
    goDown top = do
      let name = tmDefName top
      (topTp, ar) <- gets (bimap (Map.! name) (readArityWithDefault 0 name))
      modify' $ first (Map.delete name)
      (top', ars) <- lift . lift $ arityAnalysisInTopDown topTp top `runArityAnalysisDown` ar
      modify' $ second (<> ars)
      tell (Map.singleton name $ flattenNTpFun ar topTp)
      pure top'

runArityAnalysisDown :: ArityAnalysisDown a -> ArgLen -> FreshName (a, Arity)
runArityAnalysisDown g ar = runWriterT $ g `runReaderT` ar

runArityAnalysisUp :: ArityAnalysisUp a -> Context -> FreshName a
runArityAnalysisUp = runReaderT

arityAnalysisInTopDown :: Tp Val -> Top -> ArityAnalysisDown Top
arityAnalysisInTopDown topTp TopTmDef {..} = do
  tmDefBody' <- arityAnalysisInTermDown tmDefBody
  ask >>= fmap (TopTmDef tmDefName) . etaExpandDownTop topTp tmDefBody'

arityAnalysisInTopUp :: Top -> ArityAnalysisUp Top
arityAnalysisInTopUp TopTmDef {..} = TopTmDef tmDefName <$> arityAnalysisInTermUp tmDefBody

-- See note [The order of arity analysis]
arityAnalysisInTermDown :: Tm c -> ArityAnalysisDown (Tm c)
arityAnalysisInTermDown tm@(TmVar x)             = tm <$ useVar x
arityAnalysisInTermDown tm@(TmGlobal x)          = tm <$ useVar x
arityAnalysisInTermDown tm@TmUnit                = pure tm
arityAnalysisInTermDown tm@TmTrue                = pure tm
arityAnalysisInTermDown tm@TmFalse               = pure tm
arityAnalysisInTermDown tm@(TmInt _)             = pure tm
arityAnalysisInTermDown tm@(TmDouble _)          = pure tm
arityAnalysisInTermDown (TmThunk tm)             = TmThunk <$> arityAnalysisInTermDown tm
arityAnalysisInTermDown (TmIf tm0 tm1 tm2)       =
  liftA3
  TmIf
  (arityAnalysisInNonTailDown tm0)
  (arityAnalysisInBranchDown tm1)
  (arityAnalysisInBranchDown tm2)
arityAnalysisInTermDown (TmLam p tm)             = TmLam p <$> arityAnalysisInLamDown tm
arityAnalysisInTermDown (tmf `TmApp` tma)        = liftA2 TmApp (arityAnalysisInAppFunDown tmf) (arityAnalysisInNonTailDown tma)
arityAnalysisInTermDown (TmForce tm)             = TmForce <$> arityAnalysisInTermDown tm
arityAnalysisInTermDown (TmReturn tm)            = TmReturn <$> arityAnalysisInTermDown tm
arityAnalysisInTermDown (TmTo tm0 x tm1)         = do
  (tm1', ar) <- censor (deleteArity x) . listens (readArityWithDefault 0 x) $ arityAnalysisInTermDown tm1
  tm0' <- local (const ar) $ arityAnalysisInTermDown tm0
  pure $ TmTo tm0' x tm1'
arityAnalysisInTermDown (TmLet x tm0 tm1)        = do
  (tm1', ar) <- censor (deleteArity x) . listens (readArityWithDefault 0 x) $ arityAnalysisInTermDown tm1
  tm0' <- local (const ar) $ arityAnalysisInTermDown tm0
  pure $ TmLet x tm0' tm1'
arityAnalysisInTermDown (TmPrimBinOp op tm0 tm1) = liftA2 (TmPrimBinOp op) (arityAnalysisInNonTailDown tm0) (arityAnalysisInNonTailDown tm1)
arityAnalysisInTermDown (TmPrimUnOp op tm)       = TmPrimUnOp op <$> arityAnalysisInNonTailDown tm
arityAnalysisInTermDown (TmPrintInt tm0 tm1)     = liftA2 TmPrintInt (arityAnalysisInNonTailDown tm0) (arityAnalysisInTermDown tm1)
arityAnalysisInTermDown (TmPrintDouble tm0 tm1)  = liftA2 TmPrintDouble (arityAnalysisInNonTailDown tm0) (arityAnalysisInTermDown tm1)
arityAnalysisInTermDown (TmRec p tm)             = do
  let x = paramName p
      tp = paramType p
  ar <- ask
  (censor (deleteArity x) . listens (min ar . readArityWithDefault 0 x) $ arityAnalysisInTermDown tm)
    >>= fmap (TmRec . Param x $ flattenNTpFun ar tp) . uncurry (etaExpandDown tp . TmReturn . TmThunk)

arityAnalysisInNonTailDown :: Tm c -> ArityAnalysisDown (Tm c)
arityAnalysisInNonTailDown = local (const 0) . arityAnalysisInTermDown

arityAnalysisInBranchDown :: Tm c -> ArityAnalysisDown (Tm c)
arityAnalysisInBranchDown = local (const 0) . arityAnalysisInTermDown

arityAnalysisInLamDown :: Tm Com -> ArityAnalysisDown (Tm Com)
arityAnalysisInLamDown = local (max 0 . subtract 1) . arityAnalysisInTermDown

arityAnalysisInAppFunDown :: Tm c -> ArityAnalysisDown (Tm c)
arityAnalysisInAppFunDown tm = local (+ 1) $ arityAnalysisInTermDown tm

useVar :: Ident -> ArityAnalysisDown ()
useVar x = do
  argLen <- ask
  tell . Arity $ Map.singleton x argLen

etaExpandDownTop :: (MonadFreshName m) => Tp Val -> Tm Com -> ArgLen -> m (Tm Com)
etaExpandDownTop _  tm 0     = pure tm
etaExpandDownTop tp tm arity = TmReturn . TmThunk <$> etaExpandDown tp tm arity

etaExpandDown :: (MonadFreshName m) => Tp Val -> Tm Com -> ArgLen -> m (Tm Com)
etaExpandDown _  tm 0     = pure tm
etaExpandDown tp tm arity = do
  xs <- freshNamesOf (fmap (toArityVar . fromString . show)  [(0::ArgLen)..(arity - 1)])
  let apps = foldl' buildApp tm xs
  pure . foldr TmLam apps $ zipWith Param xs tpPs
  where
    tpPs = takeTpFunArg arity tp

    buildApp :: Tm Com -> Ident -> Tm Com
    buildApp tm' x = TmTo tm' tempX (TmForce (TmVar tempX) `TmApp` TmVar x)
      where
        tempX = toArityTempVar x

arityAnalysisInTermUp :: Tm c -> ArityAnalysisUp (Tm c)
arityAnalysisInTermUp tm@(TmVar x)              = asks (Map.!? x) >>= maybe (pure tm) (`etaExpandUpVar` tm)
arityAnalysisInTermUp tm@(TmGlobal x)           = asks (Map.! x) >>= (`etaExpandUpVar` tm)
arityAnalysisInTermUp tm@TmUnit                 = pure tm
arityAnalysisInTermUp tm@TmTrue                 = pure tm
arityAnalysisInTermUp tm@TmFalse                = pure tm
arityAnalysisInTermUp tm@(TmInt _)              = pure tm
arityAnalysisInTermUp tm@(TmDouble _)           = pure tm
arityAnalysisInTermUp (TmThunk tm)              = TmThunk <$> arityAnalysisInTermUp tm
arityAnalysisInTermUp (TmIf tm0 tm1 tm2)        = liftA3 TmIf (arityAnalysisInTermUp tm0) (arityAnalysisInTermUp tm1) (arityAnalysisInTermUp tm2)
arityAnalysisInTermUp (TmLam p tm)              = TmLam p <$> arityAnalysisInTermUp tm
arityAnalysisInTermUp (tmf `TmApp` tma)         = liftA2 TmApp (arityAnalysisInTermUp tmf) (arityAnalysisInTermUp tma)
arityAnalysisInTermUp (TmForce tm)              = TmForce <$> arityAnalysisInTermUp tm
arityAnalysisInTermUp (TmReturn tm)             = TmReturn <$> arityAnalysisInTermUp tm
arityAnalysisInTermUp (TmTo tm0 x tm1)          = liftA2 (`TmTo` x) (arityAnalysisInTermUp tm0) (arityAnalysisInTermUp tm1)
arityAnalysisInTermUp (TmLet x tm0 tm1)         = liftA2 (TmLet x) (arityAnalysisInTermUp tm0) (arityAnalysisInTermUp tm1)
arityAnalysisInTermUp (TmPrimBinOp op tm0 tm1)  = liftA2 (TmPrimBinOp op) (arityAnalysisInTermUp tm0) (arityAnalysisInTermUp tm1)
arityAnalysisInTermUp (TmPrimUnOp op tm)        = TmPrimUnOp op <$> arityAnalysisInTermUp tm
arityAnalysisInTermUp (TmPrintInt tm0 tm1)      = liftA2 TmPrintInt (arityAnalysisInTermUp tm0) (arityAnalysisInTermUp tm1)
arityAnalysisInTermUp (TmPrintDouble tm0 tm1)   = liftA2 TmPrintDouble (arityAnalysisInTermUp tm0) (arityAnalysisInTermUp tm1)
arityAnalysisInTermUp (TmRec p@(Param {..}) tm) =
  local (Map.insert paramName paramType) (arityAnalysisInTermUp tm)
  >>= etaExpandUp (unwrapTpUp paramType) . TmRec p

etaExpandUpVar :: (MonadFreshName m) => Tp Val -> Tm Val -> m (Tm Val)
etaExpandUpVar (TpUp tp) tm = TmThunk <$> etaExpandUp tp (TmForce tm)
etaExpandUpVar _         tm = pure tm

etaExpandUp :: (MonadFreshName m) => Tp Com -> Tm Com -> m (Tm Com)
etaExpandUp tp tm = do
  xs <- freshNamesOf (fmap (toArityVar . fromString . show)  [(0::Int)..(psLen - 1)])
  pure . foldr buildLam (foldl' TmApp tm (fmap TmVar xs)) $ zipWith Param xs tpPs
  where
    tpPs = collectDirectTpFunArg tp
    psLen = length tpPs

    buildLam :: Param -> Tm Com -> Tm Com
    buildLam p tm'@(TmLam _ _) = TmLam p (TmReturn (TmThunk tm'))
    buildLam p tm'             = TmLam p tm'

unwrapTpUp :: Tp Val -> Tp Com
unwrapTpUp (TpUp tp) = tp
unwrapTpUp _         = error "Invalid unwrapTpUp"

collectDirectTpFunArg :: Tp Com -> [Tp Val]
collectDirectTpFunArg (tpP :->: tpR) = tpP : collectDirectTpFunArg tpR
collectDirectTpFunArg _              = []

takeTpFunArg :: ArgLen -> Tp c -> [Tp Val]
takeTpFunArg 0 _              = []
takeTpFunArg n (TpUp tp)      = takeTpFunArg n tp
takeTpFunArg n (TpDown tp)    = takeTpFunArg n tp
takeTpFunArg n (tpP :->: tpR) = tpP : takeTpFunArg (n - 1) tpR
takeTpFunArg _ _              = error "impossible!"

flattenNTpFun :: ArgLen -> Tp c -> Tp c
flattenNTpFun 0 tp                           = tp
flattenNTpFun n (TpUp tp)                    = TpUp $ flattenNTpFun n tp
flattenNTpFun n (TpDown tp)                  = TpDown $ flattenNTpFun n tp
flattenNTpFun 1 (tpP :->: tpR)               = tpP :->: tpR
flattenNTpFun n (tpP :->: TpDown (TpUp tpR)) = tpP :->: flattenNTpFun (n - 1) tpR
flattenNTpFun n tp                           = error $ "Illegal number of function type flattening " <> show n <> ", " <> show tp

newtype ArgLen = ArgLen Int
  deriving newtype (Eq, Ord, Enum, Show, Num)

instance Semigroup ArgLen where
  (<>) = (+)

instance Monoid ArgLen where
  mempty = 0

newtype Arity = Arity (Map Ident ArgLen)

instance Semigroup Arity where
  (<>) = coerce (Map.unionWith min :: Map Ident ArgLen -> Map Ident ArgLen -> Map Ident ArgLen)

instance Monoid Arity where
  mempty = Arity Map.empty

deleteArity :: Ident -> Arity -> Arity
deleteArity x = coerce (Map.delete x :: Map Ident ArgLen -> Map Ident ArgLen)

readArityWithDefault :: ArgLen -> Ident -> Arity -> ArgLen
readArityWithDefault n x = coerce (Map.findWithDefault n x :: Map Ident ArgLen -> ArgLen)
