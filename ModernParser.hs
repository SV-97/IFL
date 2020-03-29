{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ModernParser where

import           Base

import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace)

import           Prelude             hiding (lex)

import           Control.Applicative (Alternative (..), liftA2, many, some)
import           Data.Functor        ((<&>))
import           Data.String         (IsString (..))

data Token =
  Tok
    { lineNo :: Integer
    , text   :: String
    }
  deriving (Show)

runParse (P p) = p

newtype Parser a =
  P ([Token] -> [(a, [Token])])

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b -- corresponds to: pApply
  fmap f (P p) = P \toks -> [(f a, toks1) | (a, toks1) <- p toks]

instance Applicative Parser where
  pure :: a -> Parser a -- corresponds to: pEmpty
  pure a = P \toks -> [(a, toks)]
  -- corresponds to: pThen
  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 combine (P p1) (P p2) =
    P \toks ->
      [(combine v1 v2, toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1]

instance Alternative Parser where
  (<|>) :: Parser a -> Parser a -> Parser a -- corresponds to pAlt
  (P p1) <|> (P p2) = P \toks -> p1 toks ++ p2 toks
  empty :: Parser a
  empty = P $ const []

instance Monad Parser where
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (P p1) >>= f =
    P \toks ->
      [ (b, toks2)
      | (a, toks1) <- p1 toks
      , (b, toks2) <-
          let P parse = f a
           in parse toks1
      ]

-- corresponds to pSat
statisfies :: (String -> Bool) -> Parser String
statisfies pred =
  P \case
    (Tok i text:toks)
      | pred text -> [(text, toks)]
      | otherwise -> []
    [] -> []

-- corresponds to pLit
lit :: String -> Parser String
lit s = statisfies (== s)

instance IsString (Parser String) where
  fromString = lit

-- corresponds to pVar
variable :: Parser String
variable =
  statisfies \case
    text@(c:cs) -> isAlpha c && text `notElem` keywords
    [] -> False

-- corresponds to pNum
num :: Parser Integer
num =
  fmap read $
  statisfies \case
    (c:cs) -> isDigit c
    [] -> False

-- corresponds to pOneOrMoreWithSep
someWithSep :: Parser a -> Parser b -> Parser [a]
someWithSep p1 p2 = (:) <$> p1 <*> many (p2 *> p1)

-- Parser Tests
helloOrGoodbye :: Parser String
helloOrGoodbye = "hello" <|> "goodbye"

greeting :: Parser (String, String)
greeting = (,) <$> helloOrGoodbye <*> variable <* lit "!"

multipleGreetings = many greeting

greetingsN = length `fmap` many greeting

-- Core Parser
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

twoCharOps = ["==", "~=", ">=", "<=", "->"]

lex :: Integer -> String -> [Token]
lex i (c1:c2:cs)
  | [c1, c2] == "||" =
    let rest = drop 1 $ dropWhile (/= '\n') cs
     in lex (i + 1) rest
  | [c1, c2] `elem` twoCharOps = Tok i [c1, c2] : lex i cs
lex i (c:cs)
  | c == '\n' = lex (i + 1) cs
  | isSpace c = lex i cs
  | isDigit c =
    let numTok = Tok i (c : tokTail)
        (tokTail, rest) = span isDigit cs
     in numTok : lex i rest
  | isAlpha c =
    let varTok = Tok i (c : tokTail)
        (tokTail, rest) = span isIdChar cs
        isIdChar c = isAlphaNum c || c == '_'
     in varTok : lex i rest
  | otherwise = Tok i [c] : lex i cs
lex _ [] = []

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . runParse program
  where
    takeFirstParse ((prog, []):_) = prog
    takeFirstParse (_:rest)       = takeFirstParse rest
    takeFirstParse _              = error "Syntax error"

parse :: String -> CoreProgram
parse = syntax . lex 1

program :: Parser CoreProgram
program = someWithSep superComb (lit ";")

curry3 f a b c = f (a, b, c)

superComb :: Parser CoreSuperCombDef
superComb = curry3 SCDef <$> variable <*> many variable <* lit "=" <*> expr

expr :: Parser CoreExpr
expr =
  ELet nonRecursive <$> (lit "let" *> defs) <* lit "in" <*> expr <|>
  ELet recursive <$> (lit "letrec" *> defs) <* lit "in" <*> expr <|>
  ECase <$> (lit "case" *> expr) <* lit "of" <*> alts <|>
  ELam <$> (lit "\\" *> many variable) <* lit "." <*> expr <|>
  expr1

defs :: Parser [(String, CoreExpr)]
defs = someWithSep def (lit ";")

def :: Parser (String, CoreExpr)
def = (,) <$> variable <* lit "=" <*> expr

alts :: Parser [CoreAlt]
alts = someWithSep alt (lit ";")
  where
    alt =
      curry3 Alter <$> (lit "<" *> num) <* lit ">" <*> many variable <* lit "->" <*>
      expr

atomicExpr :: Parser CoreExpr
atomicExpr =
  fmap EVar variable <|> fmap ENum num <|>
  EConstr <$> (lit "Pack" *> lit "{" *> num) <* lit "," <*> num <* lit "}" <|>
  lit "(" *> expr <* lit ")"

-- BinOp Parsing
data PartialExpr
  = NoOp
  | FoundOp String CoreExpr

assembleOp e1 NoOp            = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

relOp = foldl1 (<|>) (["<", "<=", "==", "~=", ">=", ">"] :: [Parser String])

expr1 :: Parser CoreExpr
expr1 = assembleOp <$> expr2 <*> expr1c

expr1c :: Parser PartialExpr
expr1c = FoundOp <$> lit "|" <*> expr1 <|> pure NoOp

expr2 :: Parser CoreExpr
expr2 = assembleOp <$> expr3 <*> expr2c

expr2c :: Parser PartialExpr
expr2c = FoundOp <$> lit "&" <*> expr2 <|> pure NoOp

expr3 :: Parser CoreExpr
expr3 = assembleOp <$> expr4 <*> expr3c

expr3c :: Parser PartialExpr
expr3c = FoundOp <$> relOp <*> expr3 <|> pure NoOp

expr4 :: Parser CoreExpr
expr4 = assembleOp <$> expr5 <*> expr4c

expr4c :: Parser PartialExpr
expr4c = FoundOp <$> (lit "+" <|> lit "-") <*> expr4 <|> pure NoOp

expr5 :: Parser CoreExpr
expr5 = assembleOp <$> expr6 <*> expr5c

expr5c :: Parser PartialExpr
expr5c = FoundOp <$> (lit "*" <|> lit "/") <*> expr5 <|> pure NoOp

expr6 = some atomicExpr <&> reverse <&> mkApChain
  where
    mkApChain :: [CoreExpr] -> CoreExpr
    mkApChain (e:es@(_:_)) = EAp (mkApChain es) e
    mkApChain [e]          = e
