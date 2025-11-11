{-# LANGUAGE ApplicativeDo #-}
module LambdaComp.External.Parser
  ( runProgramParser
  ) where

import Control.Monad                  (join, void)
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
program = sepEndBy (withSourceSpan top) (symbol ";;")

top :: Parser Top
top = topTmDef <|> topTmDecl

topTmDef :: Parser Top
topTmDef = do
  tmDefName <- try $ ident <* symbol "="
  tmDefBody <- xtm
  pure $ TopTmDef {..}

topTmDecl :: Parser Top
topTmDecl = do
  tmDefName <- try $ ident <* symbol ":"
  tmDefType <- withSourceSpan tp
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

xtm :: Parser XTm
xtm = withSourceSpan (tmLam <|> tmIf <|> tmPrintInt <|> tmPrintDouble) <|> Expr.makeExprParser atomicXTm tmTable

tmLam :: Parser Tm
tmLam = do
  ps <- join <$> some (between (symbol "\\") (symbol "->") . some $ withSourceSpan param)
  TmLam ps <$> xtm

tmIf :: Parser Tm
tmIf = do
  keyword "if"
  xtm0 <- xtm
  keyword "then"
  xtm1 <- xtm
  keyword "else"
  TmIf xtm0 xtm1 <$> xtm

tmPrintInt :: Parser Tm
tmPrintInt = do
  keyword "printInt"
  xtm0 <- xtm
  keyword "then"
  TmPrintInt xtm0 <$> xtm

tmPrintDouble :: Parser Tm
tmPrintDouble = do
  keyword "printDouble"
  xtm0 <- xtm
  keyword "then"
  TmPrintDouble xtm0 <$> xtm

tmTable :: [[Expr.Operator Parser XTm]]
tmTable =
  [ [ Expr.Prefix $ useUnaryXTm (TmPrimUnOp PrimDNeg) <$> ofSourceSpan (symbolNoSpace "-.")
    , Expr.Prefix $ useUnaryXTm (TmPrimUnOp PrimINeg) <$> ofSourceSpan (symbolNoSpace "-")
    ]
  , [ Expr.Prefix $ useUnaryXTm (TmPrimUnOp PrimIToD) <$> ofSourceSpan (keyword "intToDouble")
    , Expr.Prefix $ useUnaryXTm (TmPrimUnOp PrimDToI) <$> ofSourceSpan (keyword "doubleToInt")
    , Expr.InfixL $ addAppArgXTm <$ space
    ]
  , [ Expr.Prefix $ useUnaryXTm (TmPrimUnOp PrimBNot) <$> ofSourceSpan (symbol "~")
    ]
  , [ Expr.InfixL $ liftX2 (TmPrimBinOp PrimDMul) <$ symbol "*."
    , Expr.InfixL $ liftX2 (TmPrimBinOp PrimDDiv) <$ symbol "/."
    , Expr.InfixL $ liftX2 (TmPrimBinOp PrimIMul) <$ symbol "*"
    , Expr.InfixL $ liftX2 (TmPrimBinOp PrimIDiv) <$ symbol "/"
    , Expr.InfixL $ liftX2 (TmPrimBinOp PrimIMod) <$ symbol "%"
    ]
  , [ Expr.InfixL $ liftX2 (TmPrimBinOp PrimDAdd) <$ symbol "+."
    , Expr.InfixL $ liftX2 (TmPrimBinOp PrimDSub) <$ symbol "-."
    , Expr.InfixL $ liftX2 (TmPrimBinOp PrimIAdd) <$ symbol "+"
    , Expr.InfixL $ liftX2 (TmPrimBinOp PrimISub) <$ symbol "-"
    ]
  , [ Expr.InfixN $ liftX2 (TmPrimBinOp PrimDEq) <$ symbol "=."
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimDNEq) <$ symbol "<>."
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimDLe) <$ symbol "<=."
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimDLt) <$ symbol "<."
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimDGe) <$ symbol ">=."
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimDGt) <$ symbol ">."
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimIEq) <$ symbol "="
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimINEq) <$ symbol "<>"
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimILe) <$ symbol "<="
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimILt) <$ symbol "<"
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimIGe) <$ symbol ">="
    , Expr.InfixN $ liftX2 (TmPrimBinOp PrimIGt) <$ symbol ">"
    ]
  , [ Expr.InfixL $ liftX2 (TmPrimBinOp PrimBAnd) <$ symbol "&&"
    ]
  , [ Expr.InfixL $ liftX2 (TmPrimBinOp PrimBOr) <$ symbol "||"
    ]
  , [ Expr.Postfix $ liftX2 (flip TmAnn) <$ symbol ":" <*> withSourceSpan tp
    ]
  ]

addAppArgXTm :: XTm -> XTm -> XTm
addAppArgXTm (TmApp xtmF xtmAs, span0) xtmA@(_, span1) = (TmApp xtmF (xtmAs <> [xtmA]), mergeSourceSpan span0 span1)
addAppArgXTm xtmF                      xtmA            = (TmApp xtmF [xtmA], mergeSourceSpan (snd xtmF) (snd xtmA))

useUnaryXTm :: (XTm -> Tm) -> SourceSpan -> XTm -> XTm
useUnaryXTm f span0 xtmOp@(_, span1) = (f xtmOp, mergeSourceSpan span0 span1)

atomicXTm :: Parser XTm
atomicXTm =
  choice
  [ withSourceSpan $ TmVar <$> ident
  , withSourceSpan $ TmConst <$> tmConst
  , parened xtm
  ]

tmConst :: Parser TmConst
tmConst =
  choice
  [ TmCTrue <$ keyword "True"
  , TmCFalse <$ keyword "False"
  , TmCDouble <$> double
  , TmCInt <$> int
  , try $ parened $ pure TmCUnit
  ]

param :: Parser Param
param =
  label "parameter"
  $ (`Param` Nothing) <$> ident
  <|> parened (Param <$> ident <* symbol ":" <*> optional tp)

ofSourceSpan :: Parser () -> Parser SourceSpan
ofSourceSpan = fmap snd . withSourceSpan

withSourceSpan :: Parser a -> Parser (a, SourceSpan)
withSourceSpan p = (\startPos a endPos -> (a, SourceSpan {..})) <$> getSourcePos <*> p <*> getSourcePos

int :: Parser Int
int = label "integer" . lexeme . try $ MCL.signed (pure ()) MCL.decimal

double :: Parser Double
double = label "double precision floating number" . lexeme . try $ MCL.signed (pure ()) MCL.float

ident :: Parser Ident
ident = Ident <$> identifier

parened :: Parser a -> Parser a
parened = between (hidden $ symbol "(") (symbol ")")

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
  , "rec"
  , "intToDouble"
  , "doubleToInt"
  , "True"
  , "False"
  ]

isIdentChar0 :: Char -> Bool
isIdentChar0 = getAny . ((Any . isLower) <> (Any . ('_' ==)))

isIdentChar1 :: Char -> Bool
isIdentChar1 = getAny . ((Any . isAlphaNum) <> (Any . ('_' ==)))
