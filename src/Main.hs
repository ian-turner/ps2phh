module Main where

import System.Environment (getArgs)
import Text.Regex.TDFA
import Data.List (filter, intercalate)

import Syntax
import Parser
import Convert


badLine :: String -> Bool
badLine x =
  (x =~ ".*sits.*") ||
  (x =~ ".*sitting.*") ||
  (x =~ ".*joins.*") ||
  (x =~ ".*disconnected.*") ||
  (x =~ ".*returns.*") ||
  (x =~ ".*collected.*") ||
  (x =~ ".*HOLE.*") ||
  (x =~ ".*SHOW DOWN.*")

cleanLines :: [String] -> [String]
cleanLines lns = filter (\x -> not (badLine x)) lns

removeSummary :: [String] -> [String]
removeSummary (x:xs) =
  case x of
    "*** SUMMARY ***" -> []
    x' -> x : removeSummary xs

showHandHistory :: PokerStarsHand -> IO ()
showHandHistory (PokerStarsHand pls bs acts btn) = do
  putStrLn "BLINDS:"
  mapM_ (\x -> putStrLn $ show x) bs
  putStrLn $ "BUTTON: " ++ (show btn)
  putStrLn "\nPLAYERS:"
  mapM_ (\x -> putStrLn $ show x) $ pls
  putStrLn "\nACTIONS:"
  mapM_ (\x -> putStrLn $ show x) acts

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please provide input filename"
    (a:as) -> do
      fileContent <- readFile a
      let lns = lines fileContent
          lns' = removeSummary $ cleanLines lns
          fileContent' = intercalate "\n" lns'
      let result = parseHandHistory a fileContent'
      case result of
        Right hh -> showHandHistory hh
        Left err -> putStrLn $ show err
