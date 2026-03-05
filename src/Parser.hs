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
readCard :: Parser Card
readCard = do
  r <- anyChar
  s <- anyChar
  return $ [r,s]

readCards :: Parser [Card]
readCards = do
  string "["
  cards <- readCard `sepBy1` (char ' ')
  string "]"
  return cards

pokerStarsHand :: Parser PokerStarsHand
pokerStarsHand = do
  -- Reading header
  (sb, bb) <- handHeader
  -- Reading starting stacks
  plStacks <- many playerStack
  let pls = map fst plStacks
  let stks = map snd plStacks
  -- Reading player actions and dealing actions
  acts <- many $ try playerAction <|> dealAction
  return $ PokerStarsHand pls [sb, bb] stks acts

-- Parsing the first two lines
handHeader :: Parser (Float, Float)
handHeader = do
  string "PokerStars Hand #"
  many digit
  string ":  Hold'em No Limit ("
  sb <- readCashAmt
  string "/"
  bb <- readCashAmt
  voidLine
  voidLine
  return (sb, bb)

-- Parsing player starting stacks in file header
playerStack :: Parser (String, Float)
playerStack = do
  string "Seat "
  many1 alphaNum
  string ": "
  pl <- username
  string " ("
  stack <- readCashAmt
  voidLine
  return (pl, stack)

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
username :: Parser String
username = do many $ letter <|> digit <|> oneOf "_-"

parseHandHistory :: String -> String -> Either ParseError PokerStarsHand
parseHandHistory srcName cnt = runParser pokerStarsHand initialParserState srcName cnt
