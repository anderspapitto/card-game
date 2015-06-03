{-# LANGUAGE ScopedTypeVariables #-}

module Game.Shell where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Data.Maybe

import Game.DataTypes

step :: Int -> (String, Maybe (Int -> Interaction Identity r)) -> (String, Maybe (Int -> Interaction Identity r))
step n (_, Just f) = case runIdentity (runFreeT (f n)) of
 Free (GetUserChoice s f') -> (s, Just f')
 Pure r -> ("no more choices", Nothing)
step _ (_, Nothing) = ("no more choices", Nothing)

-- A toy computation of type GameM to play with

game :: GameM ()
game = do
  i <- getUserChoice "pick 0 or 1"
  void $ if (i > 0)
    then getUserChoice ("here's the first case " ++ show i)
    else getUserChoice ("here's the second case " ++ show i)

initial_game initial_game_state = case runIdentity $ runFreeT $ execStateT game initial_game_state of Free (GetUserChoice s f) -> (s, Just f)
