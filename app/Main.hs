module Main where

import           Data.List (group, groupBy, intersperse)
import           Tennis       (matchResultN, tennisMatchPretty,Player(..))

main :: IO ()
main = do
  matchStates <- tennisMatchPretty "Nadal" "Federrer"
  putStrLn . foldl (<>) "" . intersperse "\n" $ matchStates
