module Main where

import           Base
import           Parser
import           PrettyPrint

main :: IO ()
main = putStrLn . iDisplay $ (pprint preludeDefs :: ISeqRep)
