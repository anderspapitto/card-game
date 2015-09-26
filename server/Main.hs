module Main where

import           Control.Monad.Identity
import           Control.Monad.Trans.State
import           System.Random

import           Game.ApiServer (apiMain)
import           Game.Cards
import           Game.DataTypes
import           Game.Logic

randPerm :: StdGen -> [a] -> [a]
randPerm   _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)

baseDeck :: [Card]
baseDeck = concat
  [ replicate 10 getPaid
  , replicate 10 bear
  , replicate 10 burn3
  , replicate 10 draw3
  , replicate 5 base
  ]

deck1, deck2 :: [Card]
deck1 = randPerm (mkStdGen 5) baseDeck
deck2 = randPerm (mkStdGen 6) baseDeck

initial_game_state :: Game
initial_game_state = Game (Player (Cost 9 9 9 9 3) [] deck1 [base] (PlayerId 0))
                          (Player (Cost 9 9 9 9 3) [] deck2 [base] (PlayerId 1))
                          Nothing
                          []

initial_game :: Interaction Identity ()
initial_game = void $ execStateT runGame initial_game_state


main :: IO ()
main = apiMain initial_game
