{-# LANGUAGE OverloadedStrings #-}

module TemplateInst where

import           Base         (CoreExpr, CoreProgram, CoreSupercombDef,
                               Expr (..), SupercombDef (..), preludeDefs)
import           CorePrograms
import           ModernParser (parse)
import           PrettyPrint
import           Utils        (Addr, Assoc, Heap (..), HeapRep, aLookup, mapSnd)

import           Data.List    (mapAccumL)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStats = Integer

-- class TiStats tiStats where
tiStatInitial :: TiStats
tiStatInitial = 0

tiStatsIncSteps :: TiStats -> TiStats
tiStatsIncSteps s = s + 1

tiStatsGetSteps :: TiStats -> Integer
tiStatsGetSteps = id

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (s, d, h, g, stats) = (s, d, h, g, f stats)

type TiStack = [Addr]

data TiDump =
  DummyTiDump

initialTiDump = DummyTiDump

type TiHeap = HeapRep Node

data Node
  = NAp Addr Addr
  | NSupercomb String [String] CoreExpr
  | NNum Integer

type TiGlobals = Assoc String Addr

run :: String -> String
run = showResults . eval . compile . parse

compile :: CoreProgram -> TiState
compile program =
  (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = [adressOfMain]
    adressOfMain = aLookup globals "main" (error "main is not defined")
    extraPreludeDefs = []

buildInitialHeap :: [CoreSupercombDef] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocateSc hInitial

allocateSc :: TiHeap -> CoreSupercombDef -> (TiHeap, (String, Addr))
allocateSc heap (SCDef (name, args, body)) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates =
      if tiFinal state
        then []
        else eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatsIncSteps

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, heap, _, stats) = isDataNode $ hLookup heap soleAddr
tiFinal ([], _, _, _, _)                = error "Empty Stack!"
tiFinal state                           = False -- Stack contains multiple items

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) =
  dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Integer -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 =
  (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> String -> [String] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) scName argNames body
  | stackSize < argLength =
    error $
    "Missing  arguments for supercombinator '" ++
    scName ++
    "'. Expected " ++ show argLength ++ ", got " ++ show stackSize ++ "."
  | otherwise = (newStack, dump, newHeap, globals, stats)
  where
    newStack = resultAddr : drop (length argNames + 1) stack
    (newHeap, resultAddr) = instantiate body heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argLength = 1 + length argNames
    stackSize = length stack

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map getArg stack
  where
    getArg addr = arg
      where
        (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc String Addr -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env =
  (heap, aLookup env v (error $ "Undefined name" ++ show v))
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env =
  instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"

instantiateLet ::
     Bool
  -> [(String, CoreExpr)]
  -> CoreExpr
  -> TiHeap
  -> Assoc String Addr
  -> (TiHeap, Addr)
instantiateLet isRec defs body heap env = instantiate body heap'' env''
  where
    (env'', heap'') = foldr h (env, heap) defs
    h =
      if isRec
        then f
        else g
    f, g, h ::
         (String, CoreExpr)
      -> (Assoc String Addr, TiHeap)
      -> (Assoc String Addr, TiHeap)
    f (name, rhs) (env', heap') = (env'', heap'')
      where
        env'' = (name, addr) : env'
        (heap'', addr) = instantiate rhs heap env''
    g (name, rhs) (env', heap') = (env'', heap'')
      where
        env'' = (name, addr) : env'
        (heap'', addr) = instantiate rhs heap env'

instantiateConstr = undefined

showResults :: [TiState] -> String
showResults states = iDisplay iseq
  where
    iseq =
      iLayn [iConcat $ map showState states, showStats (last states)] :: ISeqRep

showState :: ISeq iseq => TiState -> iseq
showState (stack, dump, heap, globals, stats) =
  iConcat [showStack heap stack, iNewline]

showStack :: ISeq iseq => TiHeap -> TiStack -> iseq
showStack heap stack =
  iConcat
    ["Stk [", iIndent (iInterleave iNewline (map showStackItem stack)), "]"]
  where
    showStackItem addr =
      iConcat [showFWAddr addr, ": ", showStkNode heap (hLookup heap addr)]

showStkNode :: ISeq iseq => TiHeap -> Node -> iseq
showStkNode heap (NAp funAddr argAddr) =
  iConcat
    [ "NAp "
    , showFWAddr funAddr
    , " "
    , showFWAddr argAddr
    , " ("
    , showNode (hLookup heap argAddr)
    , ")"
    ]
showStkNode heap node = showNode node

showNode :: ISeq iseq => Node -> iseq
showNode (NAp a1 a2) = iConcat ["NAp ", showAddr a1, " ", showAddr a2]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = "NNum " <> iNum n

showAddr :: ISeq iseq => Addr -> iseq
showAddr addr = iStr $ show addr

showFWAddr :: ISeq iseq => Addr -> iseq
showFWAddr addr = iStr (spaces (4 - length str) ++ str)
  where
    str = show addr

showStats :: ISeq iseq => TiState -> iseq
showStats (stack, dump, heap, globals, stats) =
  iConcat
    [ iNewline
    , iNewline
    , "Total number of steps = "
    , iNum $ tiStatsGetSteps stats
    ]
