{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Read (readMaybe)

import Reflex
import Reflex.Dom hiding (attributes)


-- Generic helpers for datatypes
--


-- Game control flow/logic
--

runGame :: GameM ()
runGame = do
  runDraw player1 5
  runDraw player2 5
  forever (runTurn >> switchTurns)

switchTurns :: GameM ()
switchTurns = do
  -- lift $ putStrLn "*** Switching Active Player ***"
  modify (\(Game a b) -> Game b a)
  player1 . actions .= 4

getInput :: Show a => [a] -> GameM Int
getInput xs = do
  ret <- lift $ do
    putStrLn "Please select an option"
    putStrLn "0 ** No choice, display game state **"
    forM_ (zip [(1::Int)..] xs) $ \(i, x) -> putStrLn ((show i) ++ ". " ++ show x)
    fmap readMaybe getLine
  case ret of
   Just 0 -> do
     g <- get
     lift $ print g
     getInput xs
   Just n -> return (n - 1)
   Nothing -> do
     lift $ putStrLn "Invalid choice"
     getInput xs

chooseCardInHand :: GameM Card
chooseCardInHand = do
  h <- use $ player1 . hand
  n <- getInput h
  let ret = h !! n
  player1 . hand %= (\l -> take n l ++ drop (n+1) l)
  return ret

resolveCard :: Card -> GameM ()
resolveCard card = do
  paid <- pay $ card ^. cost
  forM_ (card ^. attributes) $ \attr -> case attr of
    AbilityAttr (Ability _ ability) -> ability
    HitsBoard -> player1 . board %= ([card] ++)
    _ -> return ()

chooseAction :: GameM PlayerAction
chooseAction = let options = [PlayCard, DrawCard, GainCoin] in do
  n <- getInput options
  return $ options !! n

playerAction :: GameM ()
playerAction = do
  action <- chooseAction
  case action of
   PlayCard -> playCard
   DrawCard -> runDraw player1 1
   GainCoin -> player1 . resources %= (<> (Cost 1 0 0 0))

playCard :: GameM ()
playCard = do
  card <- chooseCardInHand
  resolveCard card

runTurn :: GameM ()
runTurn = do
  a <- use $ player1 . actions
  when (a > 0) $ do
    player1 . actions -= 1
    playerAction
    runTurn

-- Card definitions
--

-- Testing stuff
--
