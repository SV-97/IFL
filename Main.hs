module Main where

import           Base
import           CorePrograms
import           ModernParser (parse)
import           PrettyPrint

import           TemplateInst
import           TemplateInst (run)
import           Utils

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
  putStrLn "\nfunctional lists Test\n"
  listsSource <- readFile "Example Core Programs/lists.core"
  putStrLn $ run listsSource
  putStrLn "\nletrec functional lists Test\n"
  letrecListsSource <- readFile "Example Core Programs/letrec_lists.core"
  putStrLn $ run letrecListsSource
  putStrLn "\nletrec Test\n"
  letrecSource <- readFile "Example Core Programs/letrec.core"
  putStrLn $ run letrecSource
