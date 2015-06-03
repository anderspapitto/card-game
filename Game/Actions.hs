{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Actions where

import Game.DataTypes

import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Read (readMaybe)
import System.Random

-- Individual actions that will be somehow tagged. They can be
-- manipulated, and they are combined into opaque GameM actions which
-- are "Abilities"
--

chooseTarget :: GameM Int
chooseTarget = do
    b <- use $ player1 . board
    i <- getUserChoice b
    return i

damage :: Int -> Int -> GameM ()
damage amount target = do
  player1 . board %= tweak
 where
  tweak l = take target l ++ [applyDamage (l !! target)] ++ drop (target + 1) l
  applyDamage card = card

pay :: Cost -> GameM Bool
pay (Cost a1 b1 c1 d1) = do
  Cost a2 b2 c2 d2  <- use $ player1 . resources
  if a1 > a2 || b1 > b2 || c1 > c2 || d1 > d2
    then return False
    else do
      player1 . resources %= (<> (Cost (-a1) (-b1) (-c1) (-d1)))
      return True

runDraw :: Lens' Game Player -> Int -> GameM ()
runDraw player n = do
  de <- use $ player . deck
  if length de < n
    then error "loss due to deck exhaustion"
    else zoom player $ do
      hand %= (++ take n de)
      deck %= drop n

-- Abilities, opaque GameM actions
--

damageTarget :: Int -> Ability
damageTarget x = Ability ("Deal " ++ show x ++ " damage") go where
  go = damage x 0
--  go = chooseTarget >>= damage x

draw :: Int -> Ability
draw x = Ability ("Draw " ++ show x ++ " cards") go where
  go = runDraw player1 3

gain :: Cost -> Ability
gain x = Ability ("Gain " ++ show x ++ " of something") go where
  go = player1 . resources %= (<> x)
