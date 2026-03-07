module Parser where

import Text.Parsec
import Control.Monad
import qualified Text.Parsec.Expr as E

import Syntax


data ParserState =
  ParserState {
    usernames :: [String]
  }

initialParserState :: ParserState
initialParserState =
  ParserState {
    usernames = []
  }

type Parser a = Parsec String ParserState a

voidLine :: Parser ()
voidLine = void (skipMany (noneOf "\n") >> (newline <|> (eof >> return '\n')))

-- Reading a cash amount string into a float
readCashAmt :: Parser Float
readCashAmt = do
  string "$"
  fl <- try decimals <|> nodecimals
  return fl
  where
    decimals = do
      dls <- many1 digit
      string "."
      cts <- many1 digit
      let amt = (read $ dls ++ "." ++ cts) :: Float
      return amt
    nodecimals = do
      dls <- many1 digit
      let amt = (read dls) :: Float
      return amt

-- Reading a card
readCard :: Parser String
readCard = do
  r <- anyChar
  s <- anyChar
  return $ [r,s]

readCards :: Parser [String]
readCards = do
  string "["
  cards <- readCard `sepBy1` (char ' ')
  string "]"
  return cards

pokerStarsHand :: Parser PokerStarsHand
pokerStarsHand = do
  -- Reading header
  (sb, bb, btn') <- handHeader
  -- Reading player information
  pls <- many newPlayer
  -- Re-ordering players so that button is last
  let pls' = reorderPlayers pls btn'
  -- Reading player actions and dealing actions
  acts <- many $ try playerAction <|> dealAction
  return $ PokerStarsHand pls' [sb, bb] acts btn'

reorderPlayers :: [Player] -> Int -> [Player]
reorderPlayers (a@(Player _ pos _):pls) btn =
  let pls' = (pls ++ [a]) in
  if pos == btn then
    pls'
  else
    reorderPlayers pls' btn

-- Parsing the first two lines
handHeader :: Parser (Float, Float, Int)
handHeader = do
  -- Parsing first line to get blinds
  string "PokerStars Hand #"
  many digit
  string ":  Hold'em No Limit ("
  sb <- readCashAmt
  string "/"
  bb <- readCashAmt
  voidLine
  -- Parsing second line to get button
  manyTill anyChar (try $ string "Seat #")
  btn <- many1 digit
  let btn' = (read btn) :: Int
  voidLine
  -- Return blinds, button
  return (sb, bb, btn')

-- Parsing player starting stacks in file header
newPlayer :: Parser Player
newPlayer = try $ do
  -- Parsing position
  string "Seat "
  seat <- many1 digit
  let pos = read seat :: Int
  string ": "
  -- Parsing username
  name <- manyTill anyChar (try $ string " (")
  modifyState (\s -> s { usernames = name:(usernames s) })
  -- Parsing stack
  stack <- readCashAmt
  voidLine
  return $ Player name pos stack

playerAction :: Parser PSAction
playerAction = do
  pl <- username
  string ": "
  act <- try smallBlind
         <|> try bigBlind
         <|> try bets
         <|> try raisesTo
         <|> try calls
         <|> try checks
         <|> try folds
         <|> try shows
         <|> mucks
  return $ PSPlayerAction pl act
  where
    smallBlind = do
      string "posts small blind"
      voidLine
      return PostsSmallBlind
    bigBlind = do
      string "posts big blind"
      voidLine
      return PostsBigBlind
    calls = do
      string "calls"
      voidLine
      return $ Calls
    checks = do
      string "checks"
      voidLine
      return $ Checks
    folds = do
      string "folds"
      voidLine
      return $ Folds
    bets = do
      string "bets "
      betAmt <- readCashAmt
      voidLine
      return $ BetsOrRaisesTo betAmt
    mucks = do
      try (string "doesn't show") <|> (string "mucks")
      voidLine
      return $ Mucks
    shows = do
      string "shows "
      cards <- readCards
      voidLine
      return $ Shows cards
    raisesTo = do
      string "raises "
      readCashAmt
      string " to "
      raiseAmt <- readCashAmt
      voidLine
      return $ BetsOrRaisesTo raiseAmt

dealAction :: Parser PSAction
dealAction = try flopCards
             <|> try turnCard
             <|> riverCard
  where
    flopCards = do
      string "*** FLOP *** "
      cards <- readCards
      voidLine
      return $ PSDealAction cards
    turnCard = do
      string "*** TURN *** "
      readCards
      string " "
      cards <- readCards
      voidLine
      return $ PSDealAction cards
    riverCard = do
      string "*** RIVER *** "
      readCards
      string " "
      cards <- readCards
      voidLine
      return $ PSDealAction cards

-- Parsing a player username
usernameHelper :: [String] -> Parser String
usernameHelper (x:[]) = string x
usernameHelper (x:xs) = try (string x) <|> usernameHelper xs

username :: Parser String
username = do --many $ letter <|> digit <|> oneOf "_-"
  s <- getState
  let us = usernames s
  u <- usernameHelper us
  return u

parseHandHistory :: String -> String -> Either ParseError PokerStarsHand
parseHandHistory srcName cnt = runParser pokerStarsHand initialParserState srcName cnt
