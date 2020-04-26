{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import           Base

import           Data.Monoid    (Monoid (..))

import           Data.List      (intersperse)

import           Data.Semigroup (Semigroup (..))
import           Data.String    (IsString (..))

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
    pprSupercombs :: ISeq iseq => [SupercombDef Name] -> iseq
    pprSupercombs = combine pprSupercomb
    pprSupercomb :: ISeq iseq => SupercombDef Name -> iseq
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
