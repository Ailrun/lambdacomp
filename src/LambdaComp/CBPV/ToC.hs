{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBPV.ToC
  ( runToC
  ) where

import Control.Monad               (zipWithM, join)
import Control.Monad.Reader        (MonadReader (local), Reader, asks, runReader)
import Control.Monad.State.Strict  (evalStateT)
import Control.Monad.Writer.Strict (WriterT (..), lift, MonadWriter (tell))
import Data.Bifunctor              (Bifunctor (..))
import Data.Functor.Identity       (Identity (..))
import Data.Functor.Product        (Product (Pair))
import Data.List                   (elemIndex)
import Data.Maybe                  (fromMaybe)
import Data.Semigroup              (Dual (..))
import Data.Set                    qualified as Set
import Data.Text                   qualified as Text

import LambdaComp.CBPV.Syntax
import LambdaComp.FreshName   (FreshNameT, freshNameOf, freshNamesOf)

runToC :: Tm Com -> String
runToC tm = showC . first (comment (show tm) :) . (`runReader` []) . (`evalStateT` 0) $ toC tm

data TopDef
  = ThunkBodyDef
    { thunkBodyName :: String
    , thunkBody     :: [String]
    , thunkEnvSize  :: Int
    }
  | TmDef
    { tmDefName :: String
    }

type WithClosure = FreshNameT (Reader [Ident])

class ToC a where
  type CData a
  toC :: a -> WithClosure (CData a)

instance ToC Program where
  type CData Program = String

  toC :: Program -> WithClosure (CData Program)
  toC tops = if withMain tops
    then
      fmap showC
      . runWriterT
      . fmap ((<> [forceThunkStmt (toVar "u_main")]) . join)
      $ traverse (WriterT . toC) tops
    else error "No main function is given!"
    where
      withMain []                        = False
      withMain (TopTmDef "u_main" _ _:_) = True
      withMain (_:ts)                    = withMain ts

instance ToC Top where
  type CData Top = ([String], Dual [TopDef])

  toC :: Top -> WithClosure (CData Top)
  toC TopTmDef {..} = runWriterT $ do
    tmDefBody' <- WriterT $ toC tmDefBody
    tell $ Dual [TmDef $ toVar tmDefName]
    pure $ tmDefBody' False (toVar tmDefName)

instance ToC (Tm Val) where
  type CData (Tm Val) = (Bool -> String -> [String], Dual [TopDef])

  toC :: Tm Val -> WithClosure (CData (Tm Val))
  toC (TmVar x)    = getVar x >>= valueOfConst Nothing
  toC TmUnit       = valueOfConst (Just ".int_item") "0"
  toC TmTrue       = valueOfConst (Just ".int_item") "1"
  toC TmFalse      = valueOfConst (Just ".int_item") "0"
  toC (TmInt n)    = valueOfConst (Just ".int_item") $ show n
  toC (TmDouble f) = valueOfConst (Just ".double_item") $ show f
  toC (TmThunk tm) = runWriterT $ do
    thunkBody <- WriterT $ local (const thunkEnvVars) $ toC tm
    uncurry (<>) . second const <$> WriterT (thunkOfCode thunkEnvSize thunkEnvVars (comment (show tm) : thunkBody))
    where
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm
      thunkEnvVars = Set.toList thunkEnv

valueOfConst :: Maybe String -> String -> WithClosure (CData (Tm Val))
valueOfConst mayMem c =
  pure
  ( \isDef y ->
      [ if isDef
        then defineConstItemStmt y (maybe c (\mem -> "{" <> mem <> " = " <> c <> "}") mayMem)
        else assignStmt (y <> fromMaybe "" mayMem) c
      ]
  , Dual []
  )

getVar :: Ident -> WithClosure String
getVar x = do
  mayInd <- asks (elemIndex x)
  pure $ case mayInd of
      Just i  -> nthEnvItem i
      Nothing -> toVar x

thunkOfCode :: Int -> [Ident] -> [String] -> WithClosure ((Bool -> String -> [String], String -> [String]), Dual [TopDef])
thunkOfCode thunkEnvSize thunkEnvVars thunkBody = do
  thunkBodyName <- freshNameOf "sys_thunk"
  let
    initializeThunk ifDef y
      | ifDef     = [defineConstItemStmt y $ "{.thunk_item = {.code = " <> thunkBodyName <> ", .env = " <> initialThunkEnv <> "}}"]
      | otherwise = [assignStmt (y <> ".thunk_item.code") thunkBodyName, assignStmt (y <> ".thunk_item.env") initialThunkEnv]
  envInitializations <- zipWithM
                        (\x idx -> (\v y -> assignStmt (nthItem (y <> ".thunk_item.env") idx) v) <$> getVar x)
                        thunkEnvVars
                        [(0 :: Int)..]
  pure ((initializeThunk, sequence envInitializations), Dual [ThunkBodyDef {..}])
  where
    initialThunkEnv
      | thunkEnvSize > 0 = "(item *) malloc(" <> show thunkEnvSize <> " * sizeof(item))"
      | otherwise        = nullPointer

instance ToC (Tm Com) where
  type CData (Tm Com) = ([String], Dual [TopDef])

  toC :: Tm Com -> WithClosure (CData (Tm Com))
  toC (TmIf tm0 tm1 tm2) = runWriterT $ do
    tm0Code <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    tm2Code <- WriterT $ toC tm2
    c <- lift $ freshNameOf "sys_c"
    pure (tm0Code True c <> [ifStmt (intItem c) tm1Code tm2Code])
  toC (TmLam x tm) = first (underScope . (globalStackPopStmt (toVar x) :)) <$> toC tm
  toC (tmf `TmApp` tma) = runWriterT $ liftA2 (<>) (globalStackPushStmt <$> WriterT (toC tma)) (WriterT $ toC tmf)
  toC (TmForce tm) = runWriterT $ do
    tmCode <- WriterT $ toC tm
    thunk <- lift $ freshNameOf "sys_t"
    pure (tmCode True thunk <> [forceThunkStmt thunk])
  toC (TmReturn tm) = runWriterT $ do
    tmCode <- WriterT $ toC tm
    pure $ tmCode False retValue
  toC (TmTo tm0 x tm1) = runWriterT $ do
    tm0Code <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    pure (tm0Code <> underScope (defineConstItemStmt (toVar x) retValue : tm1Code))
  toC (TmLet x tm0 tm1) = runWriterT $ do
    tm0Code <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    pure (underScope (tm0Code True (toVar x) <> tm1Code))
  toC (TmPrimBinOp op tm0 tm1) = runWriterT $ do
    tm0Code <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    Pair (Identity arg0) (Identity arg1) <- lift $ freshNamesOf (Pair "sys_arg0" "sys_arg1")
    opCode <- WriterT $ toC op
    pure (underScope (tm0Code True arg0 <> tm1Code True arg1 <> [opCode arg0 arg1 retValue]))
  toC (TmPrimUnOp op tm) = runWriterT $ do
    tmCode <- WriterT $ toC tm
    arg <- lift $ freshNameOf "sys_arg"
    opCode <- WriterT $ toC op
    pure (underScope (tmCode True arg <> [opCode arg retPointer]))
  toC (TmPrintInt tm0 tm1) = runWriterT $ do
    tm0Code <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    msg <- lift $ freshNameOf "sys_msg"
    pure (tm0Code True msg <> (printlnAsIntStmt msg : tm1Code))
  toC (TmRec x tm) = runWriterT $ do
    tmCode <- WriterT $ local (const thunkEnvVars) $ toC tm
    (thunkInit, inits) <- WriterT $ thunkOfCode thunkEnvSize thunkEnvVars (comment (show tm) : tmCode)
    pure (thunkInit True xVar <> inits xVar <> [forceThunkStmt xVar])
    where
      xVar = toVar x
      thunkEnv = freeVarOfTm tm
      thunkEnvSize = Set.size thunkEnv
      thunkEnvVars = Set.toList thunkEnv

instance ToC (PrimOp Binary) where
  type CData (PrimOp Binary) = (String -> String -> String -> String, Dual [TopDef])

  toC :: PrimOp Binary -> WithClosure (CData (PrimOp Binary))
  toC PrimIAdd = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " + " <> intItem arg1)
  toC PrimISub = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " - " <> intItem arg1)
  toC PrimIMul = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " * " <> intItem arg1)
  toC PrimIDiv = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " / " <> intItem arg1)
  toC PrimIMod = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " % " <> intItem arg1)
  toC PrimIEq  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " == " <> intItem arg1)
  toC PrimINEq = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " != " <> intItem arg1)
  toC PrimILt  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " < " <> intItem arg1)
  toC PrimILe  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " <= " <> intItem arg1)
  toC PrimIGt  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " > " <> intItem arg1)
  toC PrimIGe  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " >= " <> intItem arg1)
  toC PrimDAdd = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " + " <> doubleItem arg1)
  toC PrimDSub = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " - " <> doubleItem arg1)
  toC PrimDMul = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " * " <> doubleItem arg1)
  toC PrimDDiv = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (doubleItem ret) (doubleItem arg0 <> " / " <> doubleItem arg1)
  toC PrimDEq  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " == " <> doubleItem arg1)
  toC PrimDNEq = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " != " <> doubleItem arg1)
  toC PrimDLt  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " < " <> doubleItem arg1)
  toC PrimDLe  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " <= " <> doubleItem arg1)
  toC PrimDGt  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " > " <> doubleItem arg1)
  toC PrimDGe  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (doubleItem arg0 <> " >= " <> doubleItem arg1)
  toC PrimBAnd = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " && " <> intItem arg1)
  toC PrimBOr  = runWriterT . pure $ \arg0 arg1 ret -> assignStmt (intItem ret) (intItem arg0 <> " || " <> intItem arg1)

instance ToC (PrimOp Unary) where
  type CData (PrimOp Unary) = (String -> String -> String, Dual [TopDef])

  toC :: PrimOp Unary -> WithClosure (CData (PrimOp Unary))
  toC PrimINeg = runWriterT . pure $ \arg ret -> assignStmt (intItem ret) ("- " <> intItem arg)
  toC PrimDNeg = runWriterT . pure $ \arg ret -> assignStmt (doubleItem ret) ("- " <> doubleItem arg)
  toC PrimBNot = runWriterT . pure $ \arg ret -> assignStmt (intItem ret) ("! " <> intItem arg)

showC :: ([String], Dual [TopDef]) -> String
showC (mainBody, topDefs) =
  unlines
  $ [ "#include <runtime.h>"
    , ""
    ]
  <> (showTopDefPrototype <$> realTopDefs)
  <> [""]
  <> (showTopDef <$> realTopDefs)
  <> [showMain mainBody]
  where
    realTopDefs = reverse . getDual $ topDefs

showTopDefPrototype :: TopDef -> String
showTopDefPrototype ThunkBodyDef {..} = "void " <> thunkBodyName <> "(item *const env, item *const ret);"
showTopDefPrototype TmDef {..}        = "item " <> tmDefName <> ";"

showTopDef :: TopDef -> String
showTopDef ThunkBodyDef {..} =
  unlines
  $ [ "void " <> thunkBodyName <> "(" <> thunkEnvArg <> ", item *const ret)"
    , "{"
    ]
  <> thunkBody
  <> ["}"]
  where
    thunkEnvArg
      | thunkEnvSize > 0 = "item *const env"
      | otherwise        = "item *const _"
showTopDef TmDef{}           = ""

showMain :: [String] -> String
showMain mainBody =
  unlines
  $ [ "int main(void)"
    , "{"
    , "item " <> retValueVar <> ";"
    , "{"
    , "item *const " <> retPointer <> " = &" <> retValueVar <> ";"
    ]
  <> mainBody
  <> [ "}"
     , "return " <> intItem retValueVar <> ";"
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
toVar (Ident x) = "var_" <> Text.unpack x

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
