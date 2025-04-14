{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module LambdaComp.CBPV.ToC where

import Control.Monad.Reader       (MonadReader (local), Reader, asks, runReader)
import Control.Monad.State.Strict (MonadState (get), StateT, modify', evalStateT)
import Data.List                  (elemIndex)
import Data.Set (Set)
import Data.Set                   qualified as Set

import LambdaComp.CBPV.Syntax

type WithClosure = StateT Integer (Reader [Ident])

class ToC a where
  type CData a
  toC :: a -> WithClosure (CData a)

toVar :: Ident -> String
toVar (Ident x) = "var_" <> x

data TopDef
  = ThunkBodyDef
    { thunkBodyName :: String
    , thunkBody     :: String
    , withEnv       :: Bool
    }

instance ToC (Tm 'Val) where
  type CData (Tm 'Val) = ([TopDef], String, String)

  toC :: Tm 'Val -> WithClosure (CData (Tm 'Val))
  toC (TmVar x)    = do
    mayInd <- asks (elemIndex x)
    pure $ case mayInd of
      Just i  -> ([], "", "env[" <> show i <> "]")
      Nothing -> ([], "", toVar x)
  toC TmUnit       = pure ([], "", "int_item(0)")
  toC (TmInt n)    = pure ([], "", "int_item(" <> show n <> ")")
  toC (TmDouble f) = pure ([], "", "float_item(" <> show f <> ")")
  toC (TmThunk tm) = do
    (topDefs, thunkBody) <- local (const (Set.toList envVarsSet)) (toC tm)
    (thunkTopDef, inits, thunkItem) <- thunkOfCode envVarsSet thunkBody
    pure (thunkTopDef : topDefs, inits, thunkItem)
    where
      envVarsSet = freeVarOfTm tm

thunkOfCode :: Set Ident -> String -> WithClosure (TopDef, String, String)
thunkOfCode envVarsSet thunkBody = do
  i <- get
  modify' (1 +)
  let
    thunkItem = "thunk_" <> show i
    thunkBodyName = "thunk_body_" <> show i
    envVarsSize = Set.size envVarsSet
    withEnv = envVarsSize /= 0
    initialEnv = if withEnv
                 then "malloc(" <> show envVarsSize <> "* sizeof(item))"
                 else "NULL"
    initializations =
      ("item " <> thunkItem <> " = {.thunk_item = {.env = " <> initialEnv <> ", .body = " <> thunkBodyName <> "}};")
      : zipWith (\x idx -> thunkItem <> ".thunk_item.env[" <> show idx <> "] = " <> toVar x <> ";") (Set.toList envVarsSet) [(0 :: Int)..]
  pure (ThunkBodyDef {..}, unlines initializations, thunkItem)

instance ToC (Tm 'Com) where
  type CData (Tm 'Com) = ([TopDef], String)

  toC :: Tm 'Com -> WithClosure (CData (Tm 'Com))
  toC (TmLam x tm) = do
    (topDefs, cCode) <- toC tm
    pure (topDefs, "item " <> toVar x <> " = global_stack.items[--global_stack.top];\n" <> cCode)
  toC (tmf `TmApp` tma) = do
    (aTopDefs, inits, tmaCode) <- toC tma
    (fTopDefs, tmfCode) <- toC tmf
    pure (fTopDefs <> aTopDefs, unlines [inits, "global_stack.items[global_stack.top++] = " <> tmaCode <> ";", tmfCode])
  toC (TmForce tm) = do
    (topDefs, inits, tmCode) <- toC tm
    pure (topDefs, inits <> "\n" <> tmCode <> ".thunk_item.body(" <> tmCode <> ".thunk_item.env, ret);")
  toC (TmReturn tm) = do
    (topDefs, inits, tmCode) <- toC tm
    pure (topDefs, inits <> "\n" <> "*ret = " <> tmCode <> ";")
  toC (TmThen tm0 x tm1) = do
    (topDefs0, tm0Code) <- toC tm0
    (topDefs1, tm1Code) <- toC tm1
    pure (topDefs1 <> topDefs0, tm0Code <> "\nitem " <> toVar x <> " = *ret;\n" <> tm1Code)
  toC (TmPrint tm0 tm1) = do
    (topDefs0, inits, tm0Code) <- toC tm0
    (topDefs1, tm1Code) <- toC tm1
    pure (topDefs1 <> topDefs0, inits <> "printf(\"%d\\n\", " <> tm0Code <> ".int_item);\n" <> tm1Code)
  toC (TmRec x tm) = do
    (topDefs, tmCode) <- toC tm
    (thunkTopDef, inits, thunkItem) <- thunkOfCode envVarsSet tmCode
    pure (thunkTopDef : topDefs, inits <> "\nitem " <> toVar x <> " = " <> thunkItem <> ";\n" <> tmCode)
    where
      envVarsSet = freeVarOfTm tm

runToC :: Tm 'Com -> String
runToC = showC . (`runReader` []) . (`evalStateT` 0) . toC

showC :: ([TopDef], String) -> String
showC (topDefs, mainBody) = unlines (defaultHeaders <> (showTopDefPrototype <$> realTopDefs) <> (showTopDef <$> realTopDefs)) <> "\n" <> showMain mainBody
  where
    realTopDefs = reverse topDefs

defaultHeaders :: [String]
defaultHeaders =
  [ "#include <stdlib.h>"
  , "#include <stdio.h>"
  , "#define STACK_MAX 10000"
  , ""
  , "typedef struct thunk thunk;"
  , "typedef union item item;"
  , "struct stack;"
  , ""
  , "struct thunk {"
  , "  item *env;"
  , "  void (*body)(const item *env, item *ret);"
  , "};"
  , ""
  , "union item {"
  , "  int int_item;"
  , "  float float_item;"
  , "  thunk thunk_item;"
  , "};"
  , ""
  , "typedef struct stack {"
  , "  int top;"
  , "  item items[STACK_MAX];"
  , "} stack;"
  , ""
  , "stack global_stack = {0};"
  , ""
  , "item int_item(const int value)"
  , "{"
  , "  item a = {"
  , "    .int_item = value"
  , "  };"
  , "  return a;"
  , "}"
  , ""
  , "item float_item(const float value)"
  , "{"
  , "  item a = {"
  , "    .float_item = value"
  , "  };"
  , "  return a;"
  , "}"
  , ""
  ]

showTopDefPrototype :: TopDef -> String
showTopDefPrototype ThunkBodyDef {..} = "void " <> thunkBodyName <> "(const item *env, item *ret);\n"

showTopDef :: TopDef -> String
showTopDef ThunkBodyDef {..} = "void " <> thunkBodyName <> "(" <> envArg <> "item *ret)\n{\n" <> thunkBody <> "\n}\n"
  where
    envArg = if withEnv
             then "const item *env, "
             else "const item *_, "

showMain :: String -> String
showMain mainBody = "int main(void)\n{\n" <> "item retv;\n" <> "item *ret = &retv;\n" <> mainBody <> "\nreturn ret->int_item;\n}\n"
