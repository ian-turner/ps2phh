-- Syntax specification for PokerStars and .phh formats
module Syntax where

type BlindsOrStraddles = [Float]
data Player = Player String Int Float deriving Show -- username, position, stack

data Action
  = Folds
  | Calls
  | Checks
  | BetsOrRaisesTo Float
  | Mucks
  | Shows [String]
  deriving Show

data PSAction
  = PSPlayerAction String Action
  | PSDealAction [String]
  deriving Show

data PokerStarsHand = PokerStarsHand
  [Player]
  BlindsOrStraddles
  [PSAction]
  deriving Show
