module Main where

import System.Random (randomRIO, Random(randomR, random))
import Text.Read (readMaybe)

data Move = Rock | Paper | Scissors
  deriving (Show, Enum, Bounded)

instance Random Move where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                        (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g


data Result = Draw | Win | Lose
  deriving (Show, Enum)

main :: IO ()
main = putStrLn "Welcome to Rock, Paper, Scissors"
         >> playGame

playGame :: IO ()
playGame = putStrLn "\nType 0 for Rock, 1 for Paper, 2 for Scissors or 3 to quit."
            >> getLine >>= \lineIn ->
               case readMaybe lineIn of
                 Just 3 -> putStrLn "Thanks for playing." >> return ()
                 Just n
                   | n >= fromEnum (minBound :: Move) && n <= fromEnum (maxBound :: Move) -> playAI (toEnum n) >> playGame
                 _ -> putStrLn "Invalid input." >> playGame

play :: Move -> Move -> Result
play move1 move2 = toEnum $ (move1' - move2') `mod` 3
  where move1' = fromEnum move1
        move2' = fromEnum move2

randomMove :: IO Move
randomMove = randomRIO (minBound, maxBound)

playAI :: Move -> IO ()
playAI playerMove = randomMove >>= \aiMove ->
                      let result = play playerMove aiMove
                      in putStrLn $ "Your move was: " <> show playerMove
                                 <> "\nThe computer move was:" <> show aiMove
                                 <> "\nThe result is a: " <> show result

