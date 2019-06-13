module Main where

import           Data.List (group, groupBy, intersperse)
import           Tennis    (tennisMatchPretty)

main :: IO ()
main = do
  matchStates <- tennisMatchPretty "Nadal" "Federer"
  putStrLn . foldl (<>) "" . intersperse "\n" $ matchStates
