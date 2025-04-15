{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module LambdaComp.CBPV.ToC where

import Control.Monad               (zipWithM)
import Control.Monad.Reader        (MonadReader (local), Reader, asks,
                                    runReader)
import Control.Monad.State.Strict  (evalStateT)
import Control.Monad.Writer.Strict (WriterT (WriterT, runWriterT))
import Data.Bifunctor              (Bifunctor (first))
import Data.Functor.Identity       (Identity (Identity))
import Data.Functor.Product        (Product (Pair))
import Data.List                   (elemIndex)
import Data.Semigroup              (Dual (Dual, getDual))
import Data.Set                    (Set)
import Data.Set                    qualified as Set

import LambdaComp.CBPV.Syntax
import LambdaComp.FreshName        (FreshNameT, freshNamesOf)

type WithClosure = FreshNameT (Reader [Ident])

class ToC a where
  type CData a
  toC :: a -> WithClosure (CData a)

data TopDef
  = ThunkBodyDef
    { thunkBodyName  :: String
    , thunkBody      :: [String]
    , thunkEnv       :: String
    , thunkEnvLength :: Int
    }

instance ToC (Tm 'Val) where
  type CData (Tm 'Val) = (([String], String), Dual [TopDef])

  toC :: Tm 'Val -> WithClosure (CData (Tm 'Val))
  toC (TmVar x)    = getVar x >>= valueOfConst
  toC TmUnit       = valueOfConst $ intItemCons <> "(0)"
  toC (TmInt n)    = valueOfConst $ intItemCons <> "(" <> show n <> ")"
  toC (TmDouble f) = valueOfConst $ doubleItemCons <> "(" <> show f <> ")"
  toC (TmThunk tm) = runWriterT $ do
    thunkBody <- WriterT $ local (const $ Set.toList envVarsSet) $ toC tm
    first (uncurry (:)) <$> WriterT (thunkOfCode envVarsSet (comment (show tm) : thunkBody))
    where
      envVarsSet = freeVarOfTm tm

valueOfConst :: String -> WithClosure (CData (Tm 'Val))
valueOfConst c = pure (([], c), Dual [])

getVar :: Ident -> WithClosure String
getVar x = do
  mayInd <- asks (elemIndex x)
  pure $ case mayInd of
      Just i  -> nthEnvItem i
      Nothing -> toVar x

thunkOfCode :: Set Ident -> [String] -> WithClosure (((String, [String]), String), Dual [TopDef])
thunkOfCode envVarsSet thunkBody = do
  Identity thunkItem `Pair` Identity thunkBodyName `Pair` Identity thunkEnv <- freshNamesOf (Identity "thunk_" `Pair` Identity "thunk_body_" `Pair` Identity "thunk_env_")
  let
    thunkEnvLength = Set.size envVarsSet
    initialThunk = "{.thunk_item = " <> thunkBodyName <> "}"
  envInitializations <- zipWithM
                        (\x idx -> assignStmt (nthItem thunkEnv  idx) <$> getVar x)
                        (Set.toList envVarsSet)
                        [(0 :: Int)..]
  pure (((defineConstItemStmt thunkItem initialThunk, envInitializations), thunkItem), Dual [ThunkBodyDef {..}])

instance ToC (Tm 'Com) where
  type CData (Tm 'Com) = ([String], Dual [TopDef])

  toC :: Tm 'Com -> WithClosure (CData (Tm 'Com))
  toC (TmLam x tm) = first (underScope . (globalStackPopStmt (toVar x) :)) <$> toC tm
  toC (tmf `TmApp` tma) = runWriterT $ do
    (inits, globalStackPushStmt -> tmaCode) <- WriterT $ toC tma
    tmfCode <- WriterT $ toC tmf
    pure (inits <> (tmaCode : tmfCode))
  toC (TmForce tm) = runWriterT $ do
    (inits, tmCode) <- WriterT $ toC tm
    pure (inits <> [forceThunkStmt tmCode])
  toC (TmReturn tm) = runWriterT $ do
    (inits, assignStmt retValue -> tmCode) <- WriterT $ toC tm
    pure (inits <> [tmCode])
  toC (TmThen tm0 x tm1) = runWriterT $ do
    tm0Code <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    pure (tm0Code <> underScope (defineConstItemStmt (toVar x) retValue : tm1Code))
  toC (TmLet x tm0 tm1) = runWriterT $ do
    (inits, tm0Code) <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    pure (inits <> underScope (defineConstItemStmt (toVar x) tm0Code : tm1Code))
  toC (TmPrintInt tm0 tm1) = runWriterT $ do
    (inits, tm0Code) <- WriterT $ toC tm0
    tm1Code <- WriterT $ toC tm1
    pure (inits <> (printlnAsIntStmt (tm0Code <> ".int_item") : tm1Code))
  toC (TmRec x tm) = runWriterT $ do
    tmCode <- WriterT $ local (const $ Set.toList envVarsSet) $ toC tm
    ((thunkInit, inits), thunkItem) <- WriterT $ thunkOfCode envVarsSet (comment (show tm) : tmCode)
    pure (thunkInit : defineConstItemStmt (toVar x) thunkItem : inits <> [forceThunkStmt thunkItem])
    where
      envVarsSet = freeVarOfTm tm

runToC :: Tm 'Com -> String
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
showTopDefPrototype ThunkBodyDef {..} = "void " <> thunkBodyName <> "(item *const);"

showTopDef :: TopDef -> String
showTopDef ThunkBodyDef {..} =
  unlines
  $ thunkEnvDecl
  <> [ "void " <> thunkBodyName <> "(item *ret)"
     , "{"
     ]
  <> envDef
  <> thunkBody
  <> ["}"]
  where
    thunkEnvDecl = ["static item " <> thunkEnv <> "[" <> show thunkEnvLength <> "];" | thunkEnvLength /= 0]
    envDef = ["const item *env = " <> thunkEnv <> ";" | thunkEnvLength /= 0]

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
  , ""
  , "typedef void (*thunk)(item *const ret);"
  , ""
  , "inline item " <> intItemCons <> "(const int value);"
  , "inline item " <> doubleItemCons <> "(const double value);"
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

globalStackPushStmt :: String -> String
globalStackPushStmt = assignStmt . nthGlobalStackItem $ globalStack <> ".top++"

globalStackPopStmt :: String -> String
globalStackPopStmt var = defineConstItemStmt var . nthGlobalStackItem $ "--" <> globalStack <> ".top"

forceThunkStmt :: String -> String
forceThunkStmt thunk = thunk <> ".thunk_item(" <> retPointer <> ");"

printlnAsIntStmt :: String -> String
printlnAsIntStmt s = "printf(\"%d\\n\", " <> s <> ");"

nthGlobalStackItem :: String -> String
nthGlobalStackItem idx = globalStack <> ".items[" <> idx <> "]"

nthEnvItem :: Int -> String
nthEnvItem i = "env[" <> show i <> "]"

nthItem :: String -> Int -> String
nthItem arr i = arr <> "[" <> show i <> "]"

defineConstItemStmt :: String -> String -> String
defineConstItemStmt var val = "const item " <> var <> " = " <> val <> ";"

updateRetStmts :: String -> [String]
updateRetStmts val = [assignStmt retValue val]

assignStmt :: String -> String -> String
assignStmt var val = var <> " = " <> val <> ";"

underScope :: [String] -> [String]
underScope = (["{"] <>) . (<> ["}"])

toVar :: Ident -> String
toVar (Ident x) = "var_" <> x

retValue :: String
retValue = "*" <> retPointer

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
