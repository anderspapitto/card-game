{-# LANGUAGE RecursiveDo #-}

module Main where

import Game.Shell
import Game.DataTypes
import Game.Cards

import Control.Lens
import Control.Monad.Fix
import Text.Read (readMaybe)
import System.Random
import Reflex
import Reflex.Dom hiding (attributes)

randPerm :: StdGen -> [a] -> [a]
randPerm   _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)

baseDeck :: [Card]
baseDeck = concat
  [ replicate 10 getPaid
  , replicate 20 bear
  ]

deck1, deck2 :: [Card]
deck1 = randPerm (mkStdGen 5) baseDeck
deck2 = randPerm (mkStdGen 6) baseDeck

initial_game_state :: Game
initial_game_state = Game (Player (Cost 0 0 0 0) 100 [] deck1 [] 4) (Player (Cost 0 0 0 0) 100 [] deck2 [] 4)

getSelections :: MonadWidget t m => Dynamic t String -> m (Event t Int)
getSelections prompt = do
  el "div" $ dynText prompt
  inp <- el "div" $ textInput $ def & textInputConfig_inputType .~ "number"
  return $ attachDynWithMaybe (\a b -> readMaybe a) (inp ^. textInput_value) (textInputGetEnter inp)

stepGame :: (Reflex t, MonadHold t m, MonadFix m) => Event t Int -> m (Dynamic t (String, Maybe (Int -> Interaction Identity Game)))
stepGame selections = foldDyn step (initial_game initial_game_state) selections

main = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Card Game"
  rec
    selections <- getSelections prompts
    game       <- stepGame selections
    prompts    <- mapDyn fst game
  return ()
