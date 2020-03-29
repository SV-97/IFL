module Main where

import           Base
import           CorePrograms
import           ModernParser (parse)
import           PrettyPrint

pprintIO x = putStrLn . iDisplay $ (pprint x :: ISeqRep)

main :: IO ()
main = do
  putStrLn "Builtins:\n"
  putStrLn . iDisplay $ (pprint preludeDefs :: ISeqRep)
  putStrLn "\n"
  pprintIO $ parse exercise1_21
  putStrLn "\nBuiltins from file\n"
  builtinsSource <- readFile "Example Core Programs/builtins.core"
  pprintIO $ parse builtinsSource
