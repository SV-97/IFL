{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid    (Monoid (..))

import           Data.List      (intersperse)

import           Data.Semigroup (Semigroup (..))
import           Data.String    (IsString (..))

import           Data.Char      (isAlpha, isAlphaNum, isDigit, isSpace)

import           Prelude        hiding (lex)

type Name = String

data Expr binder
  = EVar Name
  | ENum Integer
  | EConstr Integer Integer
  | EAp (Expr binder) (Expr binder)
  | ELet IsRec [(binder, Expr binder)] (Expr binder)
  | ECase (Expr binder) [Alter binder]
  | ELam [binder] (Expr binder)

type CoreExpr = Expr Name

type IsRec = Bool

recursive = True

nonRecursive = False

bindersOf defns = [name | (name, rhs) <- defns]

rhssOf defns = [rhs | (name, rhs) <- defns]

newtype Alter a =
  Alter (Integer, [a], Expr a)

type CoreAlt = Alter Name

isAtomicExpr :: Expr binder -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr _        = False

type Program binder = [SuperCombDef binder]

type CoreProgram = Program Name

newtype SuperCombDef binder =
  SCDef (Name, [binder], Expr binder)

preludeDefs :: CoreProgram
preludeDefs =
  map
    SCDef
    [ ("I", ["x"], EVar "x")
    , ("K", ["x", "y"], EVar "x")
    , ("K", ["x", "y"], EVar "y")
    , ( "S"
      , ["f", "g", "x"]
      , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    , ("testId", [], ELam ["x"] (EVar "x"))
    , ("testPlus", ["x", "y"], EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    ]

class (Monoid iseq, IsString iseq) =>
      ISeq iseq
  where
  iNil :: iseq
  iNil = mempty
  iStr :: String -> iseq
  iStr = fromString
  iAppend :: iseq -> iseq -> iseq
  iAppend = (<>)
  iNewline :: iseq
  iIndent :: iseq -> iseq
  iDisplay :: iseq -> String
  iConcat :: [iseq] -> iseq
  iConcat = mconcat
  iInterleave :: iseq -> [iseq] -> iseq
  iInterleave s ss = iConcat $ intersperse s ss

pprint :: ISeq iseq => CoreProgram -> iseq
pprint = pprSupercombs
  where
    combine :: ISeq iseq => (a -> iseq) -> [a] -> iseq
    combine f as = iInterleave sep (map f as)
      where
        sep = iConcat [";", iNewline]
    pprSupercombs :: ISeq iseq => [SuperCombDef Name] -> iseq
    pprSupercombs = combine pprSupercomb
    pprSupercomb :: ISeq iseq => SuperCombDef Name -> iseq
    pprSupercomb (SCDef (name, args, expr)) =
      iConcat
        [ iInterleave " " (iStr name : map iStr args)
        , " = "
        , iIndent (pprExpr expr)
        ]
    pprDefs :: ISeq iseq => [(Name, CoreExpr)] -> iseq
    pprDefs = combine pprDef
    pprDef :: ISeq iseq => (Name, CoreExpr) -> iseq
    pprDef (name, expr) = iConcat [iStr name, " = ", iIndent (pprExpr expr)]
    pprAExpr e
      | isAtomicExpr e = pprExpr e
      | otherwise = "(" <> pprExpr e <> ")"
    pprAlt :: ISeq iseq => CoreAlt -> iseq
    pprAlt (Alter (num, vars, body)) =
      iConcat
        [ "<"
        , iStr $ show num
        , "> "
        , iInterleave " " (map iStr vars)
        , " -> "
        , pprExpr body
        ]
    pprAlts :: ISeq iseq => [CoreAlt] -> iseq
    pprAlts = combine pprAlt
    pprExpr :: ISeq iseq => CoreExpr -> iseq
    pprExpr (ENum n) = iStr $ show n
    pprExpr (EVar v) = iStr v
    pprExpr (EAp (EAp (EVar op) e1) e2)
      | op `elem` ["+", "-", "*", "/", "<", ">", "<=", ">="] =
        iConcat [pprAExpr e1, " ", iStr op, " ", pprAExpr e2]
    pprExpr (EAp e1 e2) = pprExpr e1 <> " " <> pprAExpr e2
    pprExpr (ELet isrec defs expr) =
      iConcat
        [ iStr keyword
        , iNewline
        , " "
        , iIndent (pprDefs defs)
        , iNewline
        , "in "
        , pprExpr expr
        ]
      where
        keyword =
          if isrec
            then "letrec"
            else "let"
    pprExpr (ECase value alternatives) =
      iConcat ["case ", pprAExpr value, " of", iNewline, pprAlts alternatives]
    pprExpr (ELam args body) =
      iConcat ["λ", iInterleave " " (map iStr args), ". ", pprExpr body]

iNum :: ISeq iseq => Integer -> iseq
iNum = iStr . show

-- left pad number to a specified width
iFWNum :: ISeq iseq => Integer -> Integer -> iseq
iFWNum width n = iStr (spaces (fromIntegral width - length digits) ++ digits)
  where
    digits = show n

iLayn :: ISeq iseq => [iseq] -> iseq
iLayn seqs = iConcat (zipWith layItem [1 ..] seqs)
  where
    layItem n seq = iConcat [iFWNum 4 n, ") ", iIndent seq, iNewline]

data ISeqRep
  = INil
  | IStr String
  | IAppend ISeqRep ISeqRep
  | IIndent ISeqRep
  | INewline
  deriving (Show)

instance Semigroup ISeqRep where
  INil <> b = b
  a <> INil = a
  a <> b = IAppend a b

instance Monoid ISeqRep where
  mempty = INil

instance IsString ISeqRep where
  fromString = IStr

spaces :: (Integral n, Num n) => n -> String
spaces n = take (fromIntegral n) $ cycle " "

flatten :: Int -> [(ISeqRep, Int)] -> String
flatten _ [] = ""
flatten col ((INil, _):seqs) = flatten col seqs
flatten col ((IStr s, _):seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2, indent):seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent):seqs) =
  "\n" ++ spaces indent ++ flatten indent seqs
flatten col ((IIndent seq, indent):seqs) = flatten col ((seq, col) : seqs)

instance ISeq ISeqRep where
  iIndent = IIndent
  iNewline = INewline
  iDisplay seq = flatten 0 [(seq, 0)]

-- Parser
data Token =
  Tok
    { lineNo :: Integer
    , text   :: String
    }
  deriving (Show)

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

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s = pSat (== s)

keywords = ["let", "letrec", "case", "in", "of", "Pack"]

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

main :: IO ()
main = do
  print "hey"
  putStrLn . iDisplay $ (pprint preludeDefs :: ISeqRep)
