module Utils where

import           Data.List  (deleteBy)
import           Data.Maybe (fromMaybe)

type Addr = Integer

class Heap heap where
  hInitial :: heap elem
  hAlloc :: heap elem -> elem -> (heap elem, Addr)
  hUpdate :: heap elem -> Addr -> elem -> heap elem
  hFree :: heap elem -> Addr -> heap elem
  hLookup :: heap elem -> Addr -> elem
  hAddresses :: heap elem -> [Addr]
  hSize :: heap elem -> Integer

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull = (== 0)

showAddr :: Addr -> String
showAddr a = "#" ++ show a

newtype HeapRep a =
  Heap (Integer, [Addr], [(Addr, a)])

remove :: [(Integer, a)] -> Integer -> [(Integer, a)]
remove list a = filter ((== a) . fst) list

instance Heap HeapRep where
  hInitial = Heap (0, [1 ..], [])
  hAlloc (Heap (size, next:free, cts)) n =
    (Heap (size + 1, free, (next, n) : cts), next)
  hUpdate (Heap (size, free, cts)) a n =
    Heap (size, free, (a, n) : remove cts a)
  hFree (Heap (size, free, cts)) a = Heap (size - 1, a : free, remove cts a)
  hLookup (Heap (size, free, cts)) a =
    fromMaybe (error $ "Can't find node " ++ show a ++ " in heap") $
    lookup a cts
  hAddresses (Heap (size, free, cts)) = [addr | (addr, node) <- cts]
  hSize (Heap (size, free, cts)) = size

type Assoc key value = [(key, value)]

aLookup :: Eq a => Assoc a b -> a -> b -> b
aLookup list key def = fromMaybe def $ lookup key list

aDomain list = [key | (key, value) <- list]

aRange list = [value | (key, value) <- list]

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x1, x2) = (x1, f x2)
