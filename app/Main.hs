module Main where

import           System.Random (newStdGen)
import           Tennis        (tennisMatchPretty)

main :: IO ()
main = do
  stdGen <- newStdGen
  putStrLn $ tennisMatchPretty stdGen "Nadal" "Federer"
