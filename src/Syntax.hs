-- Syntax specification for PokerStars and .phh formats
module Syntax where


type BlindsOrStraddles = [Float]
type StartingStacks = [Float]
type Card = String
type Player = String

data Act
  = Folds
  | Calls
  | Checks
  | PostsSmallBlind
  | PostsBigBlind
  | BetsOrRaisesTo Float
  | Mucks
  | Shows [Card]
  deriving Show

data PSAction
  = PSPlayerAction Player Act
  | PSDealAction [Card]
  deriving Show

data PokerStarsHand = PokerStarsHand
  [Player]
  BlindsOrStraddles
  StartingStacks
  [PSAction]
  deriving Show
