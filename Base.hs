module Base where

type Name = String

data Expr binder
  = EVar Name
  | ENum Integer
  | EConstr Integer Integer
  | EAp (Expr binder) (Expr binder)
  | ELet IsRec [(binder, Expr binder)] (Expr binder)
  | ECase (Expr binder) [Alter binder]
  | ELam [binder] (Expr binder)
  deriving (Show)

type CoreExpr = Expr Name

type IsRec = Bool

recursive = True

nonRecursive = False

bindersOf defns = [name | (name, rhs) <- defns]

rhssOf defns = [rhs | (name, rhs) <- defns]

newtype Alter a =
  Alter (Integer, [a], Expr a)
  deriving (Show)

type CoreAlt = Alter Name

isAtomicExpr :: Expr binder -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr _        = False

type Program binder = [SuperCombDef binder]

type CoreProgram = Program Name

newtype SuperCombDef binder =
  SCDef (Name, [binder], Expr binder)
  deriving (Show)

type CoreSuperCombDef = SuperCombDef String

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
