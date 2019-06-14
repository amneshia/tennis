module Main where

import           Tennis     (tennisMatchPretty)

main :: IO ()
main = do
  _ <- putStrLn "Enter the 1st Player's name:"
  p1Name <- getNameWithFallback "Nadal"
  _ <- putStrLn "Enter the 2st Player's name:"
  p2Name <- getNameWithFallback "Federer"
  matchRecords <- tennisMatchPretty p1Name p2Name
  putStrLn matchRecords
  where
    getNameWithFallback fallback = do
      n <- getLine
      case sanitize n of
        [] -> do
          _ <- putStrLn $ "No name entered. taking \'" <> fallback <> "\' as fallback."
          return fallback
        n' -> return n'
      where
        sanitize = take 15. filter (/= '\n') .filter (/= ' ')
