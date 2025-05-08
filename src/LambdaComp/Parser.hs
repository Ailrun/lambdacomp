{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module LambdaComp.Parser
  ( runProgramParser
  ) where

import Control.Monad                  (void, join)
import Control.Monad.Combinators.Expr qualified as Expr
import Data.Bifunctor                 (Bifunctor (first))
import Data.Char                      (isAlphaNum, isLower)
import Data.List.NonEmpty             (NonEmpty (..))
import Data.Semigroup                 (Any (..))
import Data.Set                       qualified as Set
import Data.Text                      (Text)
import Data.Text                      qualified as Text
import Data.Void                      (Void)
import Text.Megaparsec
import Text.Megaparsec.Char           qualified as MC
import Text.Megaparsec.Char.Lexer     qualified as MCL

import LambdaComp.External.Syntax

type Parser = Parsec Void Text

runProgramParser :: String -> Text -> Either String Program
runProgramParser filename = first errorBundlePretty . runParser program filename

program :: Parser Program
program = sepEndBy top (symbol ";;")

top :: Parser Top
top = topTmDef <|> topTmDecl

topTmDef :: Parser Top
topTmDef = do
  tmDefName <- try $ ident <* symbol "="
  tmDefBody <- tm
  pure $ TopTmDef {..}

topTmDecl :: Parser Top
topTmDecl = do
  tmDefName <- try $ ident <* symbol ":"
  tmDefType <- tp
  pure $ TopTmDecl {..}

tp :: Parser Tp
tp = Expr.makeExprParser atomicTp tpTable

atomicTp :: Parser Tp
atomicTp =
  choice
  [ TpBool <$ keyword "Bool"
  , TpInt <$ keyword "Int"
  , TpDouble <$ keyword "Double"
  , symbol "(" *>
    (TpUnit <$ symbol ")"
     <|> tp <* symbol ")")
  ]

tpTable :: [[Expr.Operator Parser Tp]]
tpTable =
  [ [ Expr.InfixR $ addFunParamTp <$ symbol "->"
    ]
  ]

addFunParamTp :: Tp -> Tp -> Tp
addFunParamTp tpP (TpFun tpPs tpR) = TpFun (tpP:tpPs) tpR
addFunParamTp tpP tpR              = TpFun [tpP] tpR

tm :: Parser Tm
tm = tmLam <|> tmIf <|> tmPrintInt <|> Expr.makeExprParser atomicTm tmTable

tmLam :: Parser Tm
tmLam =
  TmLam . join
  <$> some (between (symbol "\\") (symbol "->") (some param))
  <*> tm

tmIf :: Parser Tm
tmIf =
  TmIf <$ keyword "if"
  <*> tm <* keyword "then"
  <*> tm <* keyword "else"
  <*> tm

tmPrintInt :: Parser Tm
tmPrintInt =
  TmPrintInt <$ keyword "printInt"
  <*> tm <* keyword "before"
  <*> tm

tmTable :: [[Expr.Operator Parser Tm]]
tmTable =
  [ [ Expr.Prefix $ TmPrimUnOp PrimINeg <$ symbolNoSpace "-"
    , Expr.Prefix $ TmPrimUnOp PrimDNeg <$ symbolNoSpace "-."
    ]
  , [ Expr.InfixL $ addAppArgTm <$ space
    ]
  , [ Expr.Prefix $ TmPrimUnOp PrimBNot <$ symbol "~"
    ]
  , [ Expr.InfixL $ TmPrimBinOp PrimIMul <$ symbol "*"
    , Expr.InfixL $ TmPrimBinOp PrimIDiv <$ symbol "/"
    , Expr.InfixL $ TmPrimBinOp PrimIMod <$ symbol "%"
    , Expr.InfixL $ TmPrimBinOp PrimDMul <$ symbol "*."
    , Expr.InfixL $ TmPrimBinOp PrimDDiv <$ symbol "/."
    ]
  , [ Expr.InfixL $ TmPrimBinOp PrimIAdd <$ symbol "+"
    , Expr.InfixL $ TmPrimBinOp PrimISub <$ symbol "-"
    , Expr.InfixL $ TmPrimBinOp PrimDAdd <$ symbol "+."
    , Expr.InfixL $ TmPrimBinOp PrimDSub <$ symbol "-."
    ]
  , [ Expr.InfixN $ TmPrimBinOp PrimIEq <$ symbol "="
    , Expr.InfixN $ TmPrimBinOp PrimINEq <$ symbol "<>"
    , Expr.InfixN $ TmPrimBinOp PrimILt <$ symbol "<"
    , Expr.InfixN $ TmPrimBinOp PrimILe <$ symbol "<="
    , Expr.InfixN $ TmPrimBinOp PrimIGt <$ symbol ">"
    , Expr.InfixN $ TmPrimBinOp PrimIGe <$ symbol ">="
    , Expr.InfixN $ TmPrimBinOp PrimDEq <$ symbol "=."
    , Expr.InfixN $ TmPrimBinOp PrimDNEq <$ symbol "<>."
    , Expr.InfixN $ TmPrimBinOp PrimDLt <$ symbol "<."
    , Expr.InfixN $ TmPrimBinOp PrimDLe <$ symbol "<=."
    , Expr.InfixN $ TmPrimBinOp PrimDGt <$ symbol ">."
    , Expr.InfixN $ TmPrimBinOp PrimDGe <$ symbol ">=."
    ]
  , [ Expr.InfixL $ TmPrimBinOp PrimBAnd <$ symbol "&&"
    ]
  , [ Expr.InfixL $ TmPrimBinOp PrimBOr <$ symbol "||"
    ]
  , [ Expr.Postfix $ flip TmAnn <$ symbol ":" <*> tp
    ]
  ]

addAppArgTm :: Tm -> Tm -> Tm
addAppArgTm (TmApp tmF tmAs) tmA = TmApp tmF (tmAs <> [tmA])
addAppArgTm tmF              tmA = TmApp tmF [tmA]

atomicTm :: Parser Tm
atomicTm =
  choice
  [ TmVar <$> ident
  , TmTrue <$ keyword "True"
  , TmFalse <$ keyword "False"
  , TmInt <$> int
  , TmDouble <$> double
  , symbol "(" *>
    (TmUnit <$ symbol ")"
     <|> tm <* symbol ")")
  ]

param :: Parser Param
param =
  label "parameter"
  $ (`Param` Nothing) <$> ident
  <|> parened (Param <$> ident <* symbol ":" <*> optional tp)

parened :: Parser a -> Parser a
parened = between (symbol "(") (symbol ")")

int :: Parser Int
int = label "integer" . lexeme . try $ MCL.signed (pure ()) MCL.decimal

double :: Parser Double
double = label "double precision floating number" . lexeme . try $ MCL.signed (pure ()) MCL.float

ident :: Parser Ident
ident = Ident <$> identifier

identifier :: Parser Text
identifier = label "identifier" $ lexeme $ do
  x <- lookAhead identifierBody
  if x `notElem` keywords
    then identifierBody
    else failure (Just . Label $ 'k' :| ("eyword '" <> Text.unpack x <> "'")) Set.empty
  where
    identifierBody = Text.cons <$> satisfy isIdentChar0 <*> takeWhileP Nothing isIdentChar1

keyword :: Text -> Parser ()
keyword k =
  label ("keyword '" <> Text.unpack k <> "'")
  $ lexeme
  $ try (MC.string k >> notFollowedBy (satisfy isIdentChar1))

symbol :: Text -> Parser ()
symbol = void . MCL.symbol space

symbolNoSpace :: Text -> Parser ()
symbolNoSpace = void . try . (>> notFollowedBy space) . MCL.symbol (pure ())

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme space

space :: Parser ()
space = hidden MC.space

keywords :: [Text]
keywords =
  [ "Bool"
  , "Int"
  , "Double"
  , "if"
  , "then"
  , "else"
  , "printInt"
  , "before"
  , "rec"
  , "True"
  , "False"
  ]

isIdentChar0 :: Char -> Bool
isIdentChar0 = getAny . ((Any . isLower) <> (Any . ('_' ==)))

isIdentChar1 :: Char -> Bool
isIdentChar1 = getAny . ((Any . isAlphaNum) <> (Any . ('_' ==)))
