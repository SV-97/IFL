{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid    (Monoid (..))

import           Data.List      (intersperse)

import           Data.Semigroup (Semigroup (..))
import           Data.String    (IsString (..))

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
pprint e = ""
  where
    pprExpr (ENum n) = iStr $ show n
    pprExpr (EVar v) = iStr v
    pprExpr (EAp e1 e2) = pprExpr e1 <> " " <> pprExpr e2
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
    pprDefs defs = iInterleave sep (map pprDef defs)
      where
        sep = iConcat [";", iNewline]
    pprDef (name, expr) = iConcat [iStr name, " = ", iIndent (pprExpr expr)]
    pprAExpr e
      | isAtomicExpr e = pprExpr e
      | otherwise = "(" <> pprExpr e <> ")"

data ISeqRep
  = INil
  | IStr String
  | IAppend ISeqRep ISeqRep

instance Semigroup ISeqRep where
  INil <> b = b
  a <> INil = a
  a <> b = IAppend a b

instance Monoid ISeqRep where
  mempty = INil

instance IsString ISeqRep where
  fromString = IStr

flatten :: [ISeqRep] -> String
flatten []                       = ""
flatten (INil:seqs)              = flatten seqs
flatten (IStr s:seqs)            = s ++ flatten seqs
flatten (IAppend seq1 seq2:seqs) = flatten (seq1 : seq2 : seqs)

instance ISeq ISeqRep where
  iIndent = id
  iNewline = "\n"
  iDisplay seq = flatten [seq]

main :: IO ()
main = do
  print "crack"
