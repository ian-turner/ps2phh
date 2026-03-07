module Main where

import System.Environment (getArgs)
import Text.Regex.TDFA
import Data.List (filter, intercalate)
import qualified Data.Map as Map

import Syntax
import Parser


-- Unsafe version of map lookup
mapLookup :: Map.Map String Int -> String -> Int
mapLookup m name =
  case (Map.lookup name m) of
    Just x  -> x
    Nothing -> error $ "map lookup error: " ++ name

badLine :: String -> Bool
badLine x =
  (x =~ ".*sits.*") ||
  (x =~ ".*sitting.*") ||
  (x =~ ".*joins.*") ||
  (x =~ ".*disconnected.*") ||
  (x =~ ".*doesn't show.*") ||
  (x =~ ".*returns.*") ||
  (x =~ ".*collected.*") ||
  (x =~ ".*posts.*") ||
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
showHandHistory (PokerStarsHand pls bs acts) = do
  putStrLn "BLINDS:"
  mapM_ (\x -> putStrLn $ show x) bs
  putStrLn "\nPLAYERS:"
  mapM_ (\x -> putStrLn $ show x) $ pls
  putStrLn "\nACTIONS:"
  mapM_ (\x -> putStrLn $ show x) acts

actToString :: Action -> String
actToString Folds = "f"
actToString Checks = "cc"
actToString Calls = "cc"
actToString Mucks = "sm"
actToString (BetsOrRaisesTo amt) = "cbr " ++ (show amt)
actToString (Shows cards) = "sm " ++ (concat cards)

printAction :: Map.Map String Int -> PSAction -> IO ()
printAction _ (PSDealAction cards) = do
  putStr "  \"d db "
  mapM_ (\x -> putStr x) cards
  putStrLn "\","
printAction idxMap (PSPlayerAction name act) = do
  putStr $ "  \"p" ++ (show $ mapLookup idxMap name) ++ " "
  putStr $ actToString act
  putStrLn "\","

handHistoryToPHH :: PokerStarsHand -> IO ()
handHistoryToPHH (PokerStarsHand pls bs acts) = do
  putStrLn "variant = \"NT\""
  putStrLn $ "antes = 0"
  putStrLn $ "blinds_or_straddles = " ++ (show bs)
  putStrLn $ "min_bet = " ++ (show $ head $ tail bs)
  let names = map (\(Player name _ _) -> name) pls
  let stacks = map (\(Player _ _ stack) -> show $ stack) pls
  putStrLn $ "players = [" ++ (intercalate ", " (map (\x -> show x) names)) ++ "]"
  putStrLn $ "starting_stacks = [" ++ (intercalate ", " stacks) ++ "]"
  let plIdxs = [1..(length names)]
  let idxMap = Map.fromList $ zip names plIdxs
  putStrLn "actions = ["
  mapM_ (\x -> putStrLn ("  \"d dh p" ++ (show x) ++ " ????\",")) plIdxs
  mapM_ (printAction idxMap) acts
  putStrLn "]"

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
        Right hh -> handHistoryToPHH hh
        Left err -> putStrLn $ show err
