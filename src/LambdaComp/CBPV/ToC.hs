{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBPV.ToC where

import Control.Monad               (zipWithM)
import Control.Monad.Reader        (MonadReader (local), Reader, asks, runReader)
import Control.Monad.State.Strict  (evalStateT)
import Control.Monad.Writer.Strict (WriterT (WriterT, runWriterT), lift)
import Data.Bifunctor              (Bifunctor (first, second))
import Data.List                   (elemIndex)
import Data.Maybe                  (fromMaybe)
import Data.Semigroup              (Dual (Dual, getDual))
import Data.Set                    qualified as Set
import Data.Text                   qualified as Text

import LambdaComp.CBPV.Syntax
import LambdaComp.FreshName   (FreshNameT, freshNameOf)

type WithClosure = FreshNameT (Reader [Ident])

class ToC a where
  type CData a
  toC :: a -> WithClosure (CData a)

data TopDef
  = ThunkBodyDef
    { thunkBodyName :: String
    , thunkBody     :: [String]
    , thunkEnvSize  :: Int
    }

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
    pure (tm0Code True c <> [ifStmt (c <> ".int_item") tm1Code tm2Code])
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
  toC (TmPrintInt tm0 tm1) = runWriterT $ do
    tm0Code <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    msg <- lift $ freshNameOf "sys_msg"
    pure (tm0Code True msg <> (printlnAsIntStmt (msg <> ".int_item") : tm1Code))
  toC (TmRec x tm) = runWriterT $ do
    tmCode <- WriterT $ local (const thunkEnvVars) $ toC tm
    (thunkInit, inits) <- WriterT $ thunkOfCode thunkEnvSize thunkEnvVars (comment (show tm) : tmCode)
    pure (thunkInit True xVar <> inits xVar <> [forceThunkStmt xVar])
    where
      xVar = toVar x
      thunkEnv = freeVarOfTm tm
      thunkEnvSize = Set.size thunkEnv
      thunkEnvVars = Set.toList thunkEnv

runToC :: Tm Com -> String
runToC tm = showC . first (comment (show tm) :) . (`runReader` []) . (`evalStateT` 0) $ toC tm

showC :: ([String], Dual [TopDef]) -> String
showC (mainBody, topDefs) =
  unlines
  $ defaultHeaders
  <> (showTopDefPrototype <$> realTopDefs)
  <> [""]
  <> (showTopDef <$> realTopDefs)
  <> [showMain mainBody]
  where
    realTopDefs = reverse . getDual $ topDefs

showTopDefPrototype :: TopDef -> String
showTopDefPrototype ThunkBodyDef {..} = "void " <> thunkBodyName <> "(item *const env, item *const ret);"

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
     , "return " <> retValueVar <> ".int_item;"
     , "}"
     ]
  where
    retValueVar = "retv";

defaultHeaders :: [String]
defaultHeaders =
  [ "#include <stdlib.h>"
  , "#include <stdio.h>"
  , "#define STACK_MAX 10000"
  , ""
  , "typedef union item item;"
  , "typedef struct stack stack;"
  , "typedef struct thunk thunk;"
  , ""
  , "inline item " <> intItemCons <> "(const int value);"
  , "inline item " <> doubleItemCons <> "(const double value);"
  , ""
  , "struct thunk {"
  , "void (*code)(item *const env, item *const ret);"
  , "item *env;"
  , "};"
  , ""
  , "union item"
  , "{"
  , "int int_item;"
  , "double double_item;"
  , "thunk thunk_item;"
  , "};"
  , ""
  , "struct stack"
  , "{"
  , "int top;"
  , "item items[STACK_MAX];"
  , "};"
  , ""
  , "stack " <> globalStack <> " = {0};"
  , ""
  , "item " <> intItemCons <> "(const int value)"
  , "{"
  , defineConstItemStmt "a" "{.int_item = value}"
  , "return a;"
  , "}"
  , ""
  , "item " <> doubleItemCons <> "(const double value)"
  , "{"
  , defineConstItemStmt "a" "{.double_item = value}"
  , "return a;"
  , "}"
  , ""
  ]

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
printlnAsIntStmt s = "printf(\"%d\\n\", " <> s <> ");"

nthGlobalStackItem :: String -> String
nthGlobalStackItem idx = "(" <> globalStack <> ".items[" <> idx <> "])"

nthEnvItem :: Int -> String
nthEnvItem = nthItem "env"

nthItem :: String -> Int -> String
nthItem arr i = "(" <> arr <> "[" <> show i <> "])"

defineConstItemStmt :: String -> String -> String
defineConstItemStmt var val = "const item " <> var <> " = " <> val <> ";"

updateRetStmts :: String -> [String]
updateRetStmts val = [assignStmt retValue val]

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

intItemCons :: String
intItemCons = "int_item"

doubleItemCons :: String
doubleItemCons = "double_item"

comment :: String -> String
comment s = "/* " <> s <> " */"

nullPointer :: String
nullPointer = "NULL"
