{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module LambdaComp.CBPV.ToC
  ( runToC
  ) where

import Control.Monad               (join, zipWithM)
import Control.Monad.FreshName     (FreshNameT, freshNameOf, freshNamesOf, runFreshNameT)
import Control.Monad.Reader        (MonadReader (local), Reader, asks, runReader)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (..), lift)
import Data.Bifunctor              (Bifunctor (..))
import Data.Functor.Identity       (Identity (..))
import Data.Functor.Product        (Product (Pair))
import Data.List                   (elemIndex)
import Data.Maybe                  (fromMaybe)
import Data.Semigroup              (Dual (..))
import Data.Set                    qualified as Set
import Data.String                 (IsString (fromString))
import Data.Text                   qualified as Text

import LambdaComp.CBPV.Syntax

runToC :: Program -> String
runToC = (`runReader` []) . runFreshNameT . toC

data TopDef
  = ThunkBodyDef
    { cThunkBodyName :: String
    , cThunkBody     :: [String]
    , cThunkEnvSize  :: Int
    }
  | TmDef
    { cTmDefName :: String
    }

type WithClosure = FreshNameT (Reader [Ident])
type WithClosureAndTop = WriterT (Dual [TopDef]) WithClosure

class ToC a where
  type CData a
  toC :: a -> CData a

instance ToC Program where
  type CData Program = WithClosure String

  toC :: Program -> CData Program
  toC prog =
    fmap (showC . toGlobal . tmDefName $ last prog)
    . runWriterT
    . fmap join
    $ traverse toC prog

instance ToC Top where
  type CData Top = WithClosureAndTop [String]

  toC :: Top -> CData Top
  toC TopTmDef {..} = do
    tmDefBody' <- toC $ TmForce $ TmThunk tmDefBody
    tell $ Dual [TmDef $ toGlobal tmDefName]
    pure $ comment (show tmDefBody) : tmDefBody' <> [assignStmt (toGlobal tmDefName) retValue]

instance ToC (Tm Val) where
  type CData (Tm Val) = WithClosureAndTop (Bool -> String -> [String])

  toC :: Tm Val -> CData (Tm Val)
  toC (TmVar x)    = getVar x >>= valueOfConst Nothing
  toC (TmGlobal x) = getGlobal x >>= valueOfConst Nothing
  toC (TmConst c)  = uncurry valueOfConst $ toC c
  toC (TmThunk tm) = do
    thunkBody <- local (const thunkEnvVars) $ toC tm
    uncurry (<>) . second const <$> thunkOfCode thunkEnvSize thunkEnvVars (comment (show tm) : thunkBody)
    where
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm
      thunkEnvVars = Set.toList thunkEnv

instance ToC TmConst where
  type CData TmConst = (Maybe String, String)

  toC :: TmConst -> CData TmConst
  toC TmCUnit       = (Just ".int_item", "0")
  toC TmCTrue       = (Just ".int_item", "1")
  toC TmCFalse      = (Just ".int_item", "0")
  toC (TmCInt n)    = (Just ".int_item", show n)
  toC (TmCDouble f) = (Just ".double_item", show f)

valueOfConst :: Maybe String -> String -> CData (Tm Val)
valueOfConst mayMem c =
  pure
  $ \isDef y ->
      pure
      $ if isDef
        then defineConstItemStmt y (maybe c (\mem -> "{" <> mem <> " = " <> c <> "}") mayMem)
        else assignStmt (y <> fromMaybe "" mayMem) c

getVar :: Ident -> WithClosureAndTop String
getVar x = do
  mayInd <- asks (elemIndex x)
  pure $ case mayInd of
      Just i  -> nthEnvItem i
      Nothing -> toVar x

getGlobal :: Ident -> WithClosureAndTop String
getGlobal x = pure $ toGlobal x

thunkOfCode :: Int -> [Ident] -> [String] -> WithClosureAndTop (Bool -> String -> [String], String -> [String])
thunkOfCode cThunkEnvSize cThunkEnvVars cThunkBody = do
  cThunkBodyName <- lift $ freshNameOf $ toSys "thunk"
  let
    initializeThunk ifDef y
      | ifDef     = [defineConstItemStmt y $ "{.thunk_item = {.code = " <> cThunkBodyName <> ", .env = " <> initialThunkEnv <> "}}"]
      | otherwise = [assignStmt (y <> ".thunk_item.code") cThunkBodyName, assignStmt (y <> ".thunk_item.env") initialThunkEnv]
  envInitializations <- zipWithM
                        (\x idx -> (\v y -> assignStmt (nthItem (y <> ".thunk_item.env") idx) v) <$> getVar x)
                        cThunkEnvVars
                        [(0 :: Int)..]
  tell $ Dual [ThunkBodyDef {..}]
  pure (initializeThunk, sequence envInitializations)
  where
    initialThunkEnv
      | cThunkEnvSize > 0 = "(item *) malloc(" <> show cThunkEnvSize <> " * sizeof(item))"
      | otherwise         = nullPointer

instance ToC (Tm Com) where
  type CData (Tm Com) = WithClosureAndTop [String]

  toC :: Tm Com -> CData (Tm Com)
  toC (TmIf tm0 tm1 tm2) = do
    tm0Code <- toC tm0
    tm1Code <- toC tm1
    tm2Code <- toC tm2
    c <- lift $ freshNameOf $ toSys "c"
    pure (tm0Code True c <> [ifStmt (intItem c) tm1Code tm2Code])
  toC (TmLam p tm) = underScope . (globalStackPopStmt (toVar (paramName p)) :) <$> toC tm
  toC (tmf `TmApp` tma) = liftA2 (<>) (globalStackPushStmt <$> toC tma) (toC tmf)
  toC (TmForce tm) = do
    tmCode <- toC tm
    thunk <- lift $ freshNameOf $ toSys "t"
    pure (tmCode True thunk <> [forceThunkStmt thunk])
  toC (TmReturn tm) = do
    tmCode <- toC tm
    pure $ tmCode False retValue
  toC (TmTo tm0 x tm1) = do
    tm0Code <- toC tm0
    tm1Code <- toC tm1
    pure (tm0Code <> underScope (defineConstItemStmt (toVar x) retValue : tm1Code))
  toC (TmLet x tm0 tm1) = do
    tm0Code <- toC tm0
    tm1Code <- toC tm1
    pure (underScope (tm0Code True (toVar x) <> tm1Code))
  toC (TmPrimBinOp op tm0 tm1) = do
    tm0Code <- toC tm0
    tm1Code <- toC tm1
    Pair (Identity arg0) (Identity arg1) <- lift $ freshNamesOf (Pair (toSys <$> "arg0") (toSys <$> "arg1"))
    opCode <- lift $ toC op
    pure (underScope (tm0Code True arg0 <> tm1Code True arg1 <> [opCode arg0 arg1 retValue]))
  toC (TmPrimUnOp op tm) = do
    tmCode <- toC tm
    arg <- lift $ freshNameOf $ toSys "arg"
    opCode <- lift $ toC op
    pure (underScope (tmCode True arg <> [opCode arg retValue]))
  toC (TmPrintInt tm0 tm1) = do
    tm0Code <- toC tm0
    tm1Code <- toC tm1
    msg <- lift $ freshNameOf $ toSys "msg"
    pure (tm0Code True msg <> (printlnAsIntStmt msg : tm1Code))
  toC (TmPrintDouble tm0 tm1) = do
    tm0Code <- toC tm0
    tm1Code <- toC tm1
    msg <- lift $ freshNameOf $ toSys "msg"
    pure (tm0Code True msg <> (printlnAsDoubleStmt msg : tm1Code))
  toC (TmRec p tm) = do
    tmCode <- local (const thunkEnvVars) $ toC tm
    (thunkInit, inits) <- thunkOfCode thunkEnvSize thunkEnvVars (comment (show tm) : tmCode)
    pure (thunkInit True xVar <> inits xVar <> [forceThunkStmt xVar])
    where
      xVar = toVar (paramName p)
      thunkEnv = freeVarOfTm tm
      thunkEnvSize = Set.size thunkEnv
      thunkEnvVars = Set.toList thunkEnv

instance ToC (PrimOp Binary) where
  type CData (PrimOp Binary) = WithClosure (String -> String -> String -> String)

  toC :: PrimOp Binary -> CData (PrimOp Binary)
  toC PrimIAdd = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " + " <> intItem arg1)
  toC PrimISub = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " - " <> intItem arg1)
  toC PrimIMul = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " * " <> intItem arg1)
  toC PrimIDiv = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " / " <> intItem arg1)
  toC PrimIMod = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " % " <> intItem arg1)
  toC PrimIEq  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " == " <> intItem arg1)
  toC PrimINEq = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " != " <> intItem arg1)
  toC PrimILt  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " < " <> intItem arg1)
  toC PrimILe  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " <= " <> intItem arg1)
  toC PrimIGt  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " > " <> intItem arg1)
  toC PrimIGe  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " >= " <> intItem arg1)
  toC PrimDAdd = pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " + " <> doubleItem arg1)
  toC PrimDSub = pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " - " <> doubleItem arg1)
  toC PrimDMul = pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " * " <> doubleItem arg1)
  toC PrimDDiv = pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " / " <> doubleItem arg1)
  toC PrimDEq  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " == " <> doubleItem arg1)
  toC PrimDNEq = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " != " <> doubleItem arg1)
  toC PrimDLt  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " < " <> doubleItem arg1)
  toC PrimDLe  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " <= " <> doubleItem arg1)
  toC PrimDGt  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " > " <> doubleItem arg1)
  toC PrimDGe  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " >= " <> doubleItem arg1)
  toC PrimBAnd = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " && " <> intItem arg1)
  toC PrimBOr  = pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " || " <> intItem arg1)

instance ToC (PrimOp Unary) where
  type CData (PrimOp Unary) = WithClosure (String -> String -> String)

  toC :: PrimOp Unary -> CData (PrimOp Unary)
  toC PrimINeg = pure $ \arg ret -> assignStmt (intItem ret) ("- " <> intItem arg)
  toC PrimIToD = pure $ \arg ret -> assignStmt (doubleItem ret) ("(double)" <> intItem arg)
  toC PrimDNeg = pure $ \arg ret -> assignStmt (doubleItem ret) ("- " <> doubleItem arg)
  toC PrimDToI = pure $ \arg ret -> assignStmt (intItem ret) ("(int)" <> doubleItem arg)
  toC PrimBNot = pure $ \arg ret -> assignStmt (intItem ret) ("! " <> intItem arg)

showC :: String -> ([String], Dual [TopDef]) -> String
showC mainDef (mainBody, topDefs) =
  unlines
  $ [ "#include <runtime.h>"
    , ""
    ]
  <> (showTopDefPrototype <$> realTopDefs)
  <> [""]
  <> (showTopDef <$> realTopDefs)
  <> [showMain mainDef mainBody]
  where
    realTopDefs = reverse . getDual $ topDefs

showTopDefPrototype :: TopDef -> String
showTopDefPrototype ThunkBodyDef {..} = "void " <> cThunkBodyName <> "(item *const env, item *const ret);"
showTopDefPrototype TmDef {..}        = "item " <> cTmDefName <> ";"

showTopDef :: TopDef -> String
showTopDef ThunkBodyDef {..} =
  unlines
  $ [ "void " <> cThunkBodyName <> "(" <> cThunkEnvArg <> ", item *const ret)"
    , "{"
    ]
  <> cThunkBody
  <> ["}"]
  where
    cThunkEnvArg
      | cThunkEnvSize > 0 = "item *const env"
      | otherwise         = "item *const _"
showTopDef TmDef{}           = ""

showMain :: String -> [String] -> String
showMain mainDef mainBody =
  unlines
  $ [ "int main(void)"
    , "{"
    , "item " <> retValueVar <> ";"
    , "{"
    , "item *const " <> retPointer <> " = &" <> retValueVar <> ";"
    ]
  <> mainBody
  <> [ "}"
     , "return " <> intItem mainDef <> ";"
     , "}"
     ]
  where
    retValueVar = "retv";

globalStackPushStmt :: (Bool -> String -> [String]) -> [String]
globalStackPushStmt f = f False . nthGlobalStackItem $ globalStack <> ".top++"

globalStackPopStmt :: String -> String
globalStackPopStmt var = defineConstItemStmt var . nthGlobalStackItem $ "--" <> globalStack <> ".top"

forceThunkStmt :: String -> String
forceThunkStmt thunk = thunk <> ".thunk_item.code(" <> thunk <> ".thunk_item.env, " <> retPointer <> ");"

ifStmt :: String -> [String] -> [String] -> String
ifStmt c b1 b2 =
  unlines
  $ [ "if (" <> c <> ")"
    , "{"
    ]
  <> b1
  <> ("}" : "else" : "{" : b2)
  <> ["}"]

printlnAsIntStmt :: String -> String
printlnAsIntStmt s = "printf(\"%d\\n\", " <> intItem s <> ");"

printlnAsDoubleStmt :: String -> String
printlnAsDoubleStmt s = "printf(\"%.15g\\n\", " <> doubleItem s <> ");"

nthGlobalStackItem :: String -> String
nthGlobalStackItem idx = "(" <> globalStack <> ".items[" <> idx <> "])"

nthEnvItem :: Int -> String
nthEnvItem = nthItem "env"

nthItem :: String -> Int -> String
nthItem arr i = "(" <> arr <> "[" <> show i <> "])"

defineConstItemStmt :: String -> String -> String
defineConstItemStmt var val = "const item " <> var <> " = " <> val <> ";"

assignStmt :: String -> String -> String
assignStmt var val = var <> " = " <> val <> ";"

underScope :: [String] -> [String]
underScope = id -- (["{"] <>) . (<> ["}"])

toVar :: Ident -> String
toVar (toVarIdent -> Ident x) = Text.unpack x

toGlobal :: Ident -> String
toGlobal (toGlobalIdent -> Ident x) = Text.unpack x

toSys :: String -> String
toSys (toLowSysVar . Ident . fromString -> Ident x) = Text.unpack x

retValue :: String
retValue = "(*" <> retPointer <> ")"

retPointer :: String
retPointer = "ret"

globalStack :: String
globalStack = "global_stack"

intItem :: String -> String
intItem = (<> ".int_item")

doubleItem :: String -> String
doubleItem = (<> ".double_item")

comment :: String -> String
comment s = "/* " <> s <> " */"

nullPointer :: String
nullPointer = "NULL"
