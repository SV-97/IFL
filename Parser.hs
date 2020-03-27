{-# LANGUAGE LambdaCase #-}

module Parser
  (
  ) where

import           Base

import           Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

import           Prelude   hiding (lex)

data Token =
  Tok
    { lineNo :: Integer
    , text   :: String
    }
  deriving (Show)

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar =
  pSat $ \case
    text@(c:cs) -> isAlpha c && text `notElem` keywords
    [] -> False

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
  [(combine v1 v2, toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (combine v1 v2 v3, toks3)
  | (v1, toks1) <- p1 toks
  , (v2, toks2) <- p2 toks1
  , (v3, toks3) <- p3 toks2
  ]

pThen4 ::
     (a -> b -> c -> d -> e)
  -> Parser a
  -> Parser b
  -> Parser c
  -> Parser d
  -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
  [ (combine v1 v2 v3 v4, toks4)
  | (v1, toks1) <- p1 toks
  , (v2, toks2) <- p2 toks1
  , (v3, toks3) <- p3 toks2
  , (v4, toks4) <- p4 toks3
  ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p toks = take 1 (pars toks)
  where
    pars = pOneOrMore p `pAlt` pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p toks = take 1 $ pThen (:) p (pZeroOrMore p) toks

pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f a, toks1) | (a, toks1) <- p toks]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pOneOrMore (pThen const p1 p2)

pSat :: (String -> Bool) -> Parser String
pSat pred (Tok i text:toks)
  | pred text = [(text, toks)]
  | otherwise = []
pSat _ [] = []

pNum :: Parser Integer
pNum = pApply sat read
  where
    sat =
      pSat $ \case
        (c:cs) -> isDigit c
        [] -> False

-- Parser Tests
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" `pAlt` pLit "goodbye"

pGreeting :: Parser (String, String)
pGreeting = pThen3 mkGreeting pHelloOrGoodbye pVar (pLit "!")
  where
    mkGreeting hg name exclamation = (hg, name)

greetingTest = pGreeting $ map (Tok 0) ["goodbye", "James", "!"]

pMultipleGreetings = pZeroOrMore pGreeting

multipleGreetingsTest =
  pMultipleGreetings $
  map
    (Tok 0)
    ["goodbye", "James", "!", "hello", "John", "!", "goodbye", "Alice", "!"]

pGreetingsN = pZeroOrMore pGreeting `pApply` length

greetingsNTest =
  pGreetingsN $
  map
    (Tok 0)
    ["goodbye", "James", "!", "hello", "John", "!", "goodbye", "Alice", "!"]

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
syntax = undefined

parse :: String -> CoreProgram
parse = syntax . lex 1
