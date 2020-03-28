module Main where

import           Base
import           CorePrograms
import           Parser
import           PrettyPrint

pprintIO x = putStrLn . iDisplay $ (pprint x :: ISeqRep)

main :: IO ()
main = do
  putStrLn "Builtins:\n"
  putStrLn . iDisplay $ (pprint preludeDefs :: ISeqRep)
  putStrLn "\n"
  pprintIO $ parse exercise1_21
