{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Logic where

import Data.Monoid
import Control.Lens
import Control.Monad

import Game.DataTypes
import Game.Actions

runGame :: GameM ()
runGame = do
  runDraw active 5
  runDraw inactive 5
  (runTurn >> switchTurns)

switchTurns :: GameM ()
switchTurns = do
  a <- use active
  b <- use inactive
  active .= b
  inactive .= a
  active . resources . actions .= 3

chooseCardInHand :: GameM Int
chooseCardInHand = do
  h <- use $ active . hand
  getUserChoice h

resolveCard :: Int -> GameM ()
resolveCard n = do
  card <- fmap (!! n) $ use $ active . hand
  paid <- pay $ card ^. cost
  when paid $ do
    active . hand %= (\l -> take n l ++ drop (n+1) l)
    forM_ (card ^. attributes) $ \attr -> case attr of
      AbilityAttr (Ability _ ability) -> ability
      HitsBoard -> active . board %= ([card] ++)
      _ -> return ()

chooseAction :: GameM PlayerAction
chooseAction = let options = [PlayCard, DrawCard, GainCoin] in do
  n <- getUserChoice options
  return $ options !! n

playerAction :: GameM ()
playerAction = do
  action <- chooseAction
  case action of
   PlayCard -> playCard
   DrawCard -> runDraw active 1
   GainCoin -> active . resources %= (<> (mempty { _gold = 1 }))

playCard :: GameM ()
playCard = do
  card <- chooseCardInHand
  resolveCard card

runTurn :: GameM ()
runTurn = do
  a <- use $ active . resources . actions
  when (a > 0) $ do
    playerAction
    active . resources . actions -= 1
    runTurn
