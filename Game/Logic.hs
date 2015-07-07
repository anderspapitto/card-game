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
--  logMessage "game is about to start"
  forever (runTurn >> switchTurns)


switchTurns :: GameM ()
switchTurns = do
  a <- use active
  b <- use inactive
  active .= b
  inactive .= a
  active . resources . actions .= 3

resolveCard :: Int -> GameM ()
resolveCard n = do
  card <- fmap (!! n) $ use $ active . hand
  paid <- pay $ card ^. cost
  when paid $ do
    active . hand %= (\l -> take n l ++ drop (n+1) l)
    forM_ (card ^. effects) $ \effect -> effect ^. runAbility
    when (card ^. hitsBoard) $
      active . board %= (card :)

chooseAction :: GameM PlayerAction
chooseAction = let options = [PlayCard, DrawCard, GainCoin, SelectCardOnBoard]
               in fmap (options !!) (getUserChoice options)

playerAction :: GameM ()
playerAction = do
  action <- chooseAction
  case action of
   PlayCard -> playCard
   DrawCard -> runDraw active 1
   GainCoin -> active . resources %= (<> (mempty { _gold = 1 }))
   SelectCardOnBoard -> do
     use (active . board) >>= getUserChoice >>= useCardOnBoard

useCardOnBoard :: Int -> GameM ()
useCardOnBoard i = do
  card <- (fmap (!! i)) $ use (active . board)
  let choices = (Ability "cancel" (return ())) : (card ^. abilities)
  n <- getUserChoice choices
  (choices !! n) ^. runAbility

playCard :: GameM ()
playCard = use (active . hand) >>= getUserChoice >>= resolveCard

runTurn :: GameM ()
runTurn = do
  a <- use $ active . resources . actions
  when (a > 0) $ do
    playerAction
    active . resources . actions -= 1
    runTurn
