module Tennis
  ( tennisMatchPretty
  , tennisMatch
  ) where

import           System.Random (randomRIO)
import Data.List (intersperse)

data Player
  = P1
  | P2
  deriving (Eq)

data Point
  = Zero
  | Fifteen
  | Thirty
  | Forty
  deriving (Enum)

instance Show Point where
  show Zero    = "0"
  show Fifteen = "15"
  show Thirty  = "30"
  show Forty   = "40"

data OngoingMatch
  = Score Point Point
  | Deuce
  | Advantage Player

data Match
  = Ongoing OngoingMatch
  | Over Player

tennisMatchPretty :: String -> String -> IO String
tennisMatchPretty p1 p2 = do
  states <- tennisMatch
  return . foldl (<>) "\n" . intersperse "\n" . prettify $ states
  where
    presenter = present p1 p2
    prettify = ("Start!" :) . fmap presenter

tennisMatch :: IO [Match]
tennisMatch = play initialScore
  where
    initialScore = Score Zero Zero

play :: OngoingMatch -> IO [Match]
play on = do
  s <- next on
  case s of
    Ongoing on' -> do
      ss <- play on'
      return $ s : ss
    Over _ -> return [s]

next :: OngoingMatch -> IO Match
next (Score point1 point2) = do
  p <- randomPlayer
  return $
    case (p, point1, point2) of
      (P1, Thirty, Forty) -> Ongoing Deuce
      (P2, Forty, Thirty) -> Ongoing Deuce
      (P1, Forty, _)      -> Over P1
      (P2, _, Forty)      -> Over P2
      (P1, _, _)          -> Ongoing $ Score (succ point1) point2
      (P2, _, _)          -> Ongoing $ Score point1 (succ point2)
next Deuce = do
  p <- randomPlayer
  return . Ongoing . Advantage $ p
next (Advantage p) = do
  p' <- randomPlayer
  return $
    if p == p'
      then Over p
      else Ongoing Deuce

randomPlayer :: IO Player
randomPlayer = do
  num <- randomRIO (0, 1) :: IO Int
  return $
    if num == 0
      then P1
      else P2

{-
  Presentation functions
-}
present :: String -> String -> Match -> String
present p1Name p2Name (Ongoing (Score point1 point2)) =
  namePoint p1Name point1 <> " - " <> namePoint p2Name point2
  where
    namePoint name point = name <> ": " <> show point
present p1Name p2Name (Ongoing Deuce) = "Deuce!"
present p1Name p2Name (Ongoing (Advantage p)) = advPlayer <> " has the advantage!"
  where
    advPlayer = playerName p1Name p2Name p
present p1Name p2Name (Over p) = winner <> " wins!"
  where
    winner = playerName p1Name p2Name p

playerName :: String -> String -> Player -> String
playerName p1Name _ P1 = p1Name
playerName _ p2Name P2 = p2Name
