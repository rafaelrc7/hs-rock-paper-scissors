module Main where

import System.Random (randomRIO)
import Text.Read (readMaybe)

data Move = Rock | Paper | Scissors
  deriving (Show, Enum)

data Result = Draw | Win | Lose
  deriving (Show, Enum)

main :: IO ()
main = putStrLn "Welcome to Rock, Paper, Scissors"
         >> playGame

playGame :: IO ()
playGame = putStrLn "Type 0 for Rock, 1 for Paper, 2 for Scissors or 3 to quit."
            >> getLine >>= \lineIn ->
              case readMaybe lineIn of
                Just 3 -> putStrLn "Thanks for playing." >> return ()
                Just n
                  | n >= 0 && n <= 3 -> playAI (toEnum n) >> playGame
                _ -> putStrLn "Invalid input." >> playGame

play :: Move -> Move -> Result
play move1 move2 = toEnum $ (move1' - move2') `mod` 3
  where move1' = fromEnum move1
        move2' = fromEnum move2

randomMove :: IO Move
randomMove = toEnum <$> randomRIO (0, 2)

playAI :: Move -> IO ()
playAI playerMove = randomMove >>= \aiMove ->
                      putStrLn $ "Your move was: " <> show playerMove
                          <> "\nThe computer move was:" <> show aiMove
                          <> "\nThe result is a: " <> (show $ play playerMove aiMove)

