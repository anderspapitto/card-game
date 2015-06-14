{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Actions where

import Game.DataTypes

import Data.Maybe
import Data.Monoid
import Control.Lens

foo :: (Card -> Maybe Card) -> GameM ()
foo f = do
  (active . board) %= catMaybes . map f
  return ()

chooseTarget :: GameM Int
chooseTarget = do
  i <- getUserChoice ["Active Player", "Inactive Player"]
  p <- use (if i == 0 then active else inactive)
  getUserChoice (p ^. board)

damage :: Int -> Int -> GameM ()
damage amount target = do
  active . board %= tweak
 where
  tweak l = take target l ++ [applyDamage (l !! target)] ++ drop (target + 1) l
  applyDamage card = card { _attributes = [reduceHealth attr | attr <- card ^. attributes] }
  reduceHealth attr = case attr of
    Health total current -> Health total (current - amount)
    x -> x

pay :: Cost -> GameM Bool
pay (Cost a1 b1 c1 d1 e1) = do
  Cost a2 b2 c2 d2 e2  <- use $ active . resources
  if a1 > a2 || b1 > b2 || c1 > c2 || d1 > d2 || e1 > e2
    then return False
    else do
      active . resources %= (<> (Cost (-a1) (-b1) (-c1) (-d1) (-e1)))
      return True

runDraw :: Lens' Game Player -> Int -> GameM ()
runDraw player n = do
  de <- use $ player . deck
  if length de < n
    then loseGame "deck exhaustion"
    else zoom player $ do
      hand %= (++ take n de)
      deck %= drop n

loseGame :: String -> GameM ()
loseGame _ = do
  a <- use (active . playerId)
  winner .= Just a

-- Abilities, opaque GameM actions
--

damageTarget :: Int -> Ability
damageTarget x = Ability ("Deal " ++ show x ++ " damage") go where
  go = chooseTarget >>= damage x

draw :: Int -> Ability
draw x = Ability ("Draw " ++ show x ++ " cards") go where
  go = runDraw active 3

gain :: Cost -> Ability
gain x = Ability ("Gain " ++ show x ++ " of something") go where
  go = active . resources %= (<> x)
