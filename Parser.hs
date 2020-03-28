{-# LANGUAGE LambdaCase #-}

module Parser where

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

pThen5 ::
     (a -> b -> c -> d -> e -> f)
  -> Parser a
  -> Parser b
  -> Parser c
  -> Parser d
  -> Parser e
  -> Parser f
pThen5 combine p1 p2 p3 p4 p5 toks =
  [ (combine v1 v2 v3 v4 v5, toks5)
  | (v1, toks1) <- p1 toks
  , (v2, toks2) <- p2 toks1
  , (v3, toks3) <- p3 toks2
  , (v4, toks4) <- p4 toks3
  , (v5, toks5) <- p5 toks4
  ]

pThen6 ::
     (a -> b -> c -> d -> e -> f -> g)
  -> Parser a
  -> Parser b
  -> Parser c
  -> Parser d
  -> Parser e
  -> Parser f
  -> Parser g
pThen6 combine p1 p2 p3 p4 p5 p6 toks =
  [ (combine v1 v2 v3 v4 v5 v6, toks6)
  | (v1, toks1) <- p1 toks
  , (v2, toks2) <- p2 toks1
  , (v3, toks3) <- p3 toks2
  , (v4, toks4) <- p4 toks3
  , (v5, toks5) <- p5 toks4
  , (v6, toks6) <- p6 toks5
  ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p toks = pars toks
  where
    pars = pOneOrMore p `pAlt` pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f a, toks1) | (a, toks1) <- p toks]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pZeroOrMore (pThen (flip const) p2 p1))

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
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []):_) = prog
    takeFirstParse (_:rest)       = takeFirstParse rest
    takeFirstParse _              = error "Syntax error"

parse :: String -> CoreProgram
parse = syntax . lex 1

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSuperComb (pLit ";")

pSuperComb :: Parser CoreSuperCombDef
pSuperComb = pThen4 mkSuperComb pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mkSuperComb name vars equals body = SCDef (name, vars, body)

pExpr :: Parser CoreExpr
pExpr = pLet `pAlt` pLetRec `pAlt` pCase `pAlt` pLam `pAlt` pAExpr `pAlt` pExpr1
  where
    pLet =
      pThen4
        (\_let defs _in expr -> ELet nonRecursive defs expr)
        (pLit "let")
        pDefs
        (pLit "in")
        pExpr
    pLetRec =
      pThen4
        (\_let defs _in expr -> ELet recursive defs expr)
        (pLit "letrec")
        pDefs
        (pLit "in")
        pExpr
    pCase =
      pThen4
        (\_case expr _of alts -> ECase expr alts)
        (pLit "case")
        pExpr
        (pLit "of")
        pAlts
    pLam =
      pThen4
        (\backslash args dot body -> ELam args body)
        (pLit "\\")
        (pZeroOrMore pVar)
        (pLit ".")
        pExpr

pDefs :: Parser [(String, CoreExpr)]
pDefs = pOneOrMoreWithSep pDef (pLit ";")

pDef :: Parser (String, CoreExpr)
pDef = pThen3 (\var equals expr -> (var, expr)) pVar (pLit "=") pExpr

pAlts :: Parser [CoreAlt]
pAlts = pOneOrMoreWithSep pAlt (pLit ";")
  where
    pAlt =
      pThen6
        (\c1 n1 c2 vars arrow expr -> Alter (n1, vars, expr))
        (pLit "<")
        pNum
        (pLit ">")
        (pZeroOrMore pVar)
        (pLit "->")
        pExpr

-- Parser for Atomic Expressions
pAExpr :: Parser CoreExpr
pAExpr =
  (pVar `pApply` EVar) `pAlt` (pNum `pApply` ENum) `pAlt` pConstructor `pAlt`
  pParenExpr
  where
    pConstructor =
      pThen6
        (\pack b1 n1 comma n2 b2 -> EConstr n1 n2)
        (pLit "Pack")
        (pLit "{")
        pNum
        (pLit ",")
        pNum
        (pLit "}")
    pParenExpr = pThen3 (\b1 expr b2 -> expr) (pLit "(") pExpr (pLit ")")

--BinOp Parsing
data PartialExpr
  = NoOp
  | FounOp String CoreExpr

assembleOp e1 NoOp           = e1
assembleOp e1 (FounOp op e2) = EAp (EAp (EVar op) e1) e2

pRelOp = foldl1 pAlt $ map pLit ["<", "<=", "==", "~=", ">=", ">"]

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

pExpr1c :: Parser PartialExpr
pExpr1c = pThen FounOp (pLit "|") pExpr1 `pAlt` pEmpty NoOp

pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr2c = pThen FounOp (pLit "&") pExpr2 `pAlt` pEmpty NoOp

pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr3c = pThen FounOp pRelOp pExpr3 `pAlt` pEmpty NoOp

pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr4c = pThen FounOp (pLit "+" `pAlt` pLit "-") pExpr4 `pAlt` pEmpty NoOp

pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr5c = pThen FounOp (pLit "*" `pAlt` pLit "/") pExpr5 `pAlt` pEmpty NoOp

pExpr6 = pAp
  where
    pAp = (pOneOrMore pAExpr `pApply` reverse) `pApply` mkApChain
    mkApChain :: [CoreExpr] -> CoreExpr
    mkApChain (e:es@(_:_)) = EAp (mkApChain es) e
    mkApChain [e]          = e
