{-# LANGUAGE ScopedTypeVariables #-}

module Game.Shell where

import Control.Monad.Trans.Free
import Control.Monad.Identity

import Game.DataTypes

type DisplayInfo = Game

type Foo r = ([String], Maybe DisplayInfo, Maybe (Int -> Interaction Identity r))

step :: Int -> Foo r -> Foo r
step n (_, _, Just f) = case runIdentity (runFreeT (f n)) of
 Free (GetUserChoice s d f') -> (s, Just d, Just f')
 Pure _ -> (["no more choices"], Nothing, Nothing)
step _ (_, _, Nothing) = (["no more choices"], Nothing, Nothing)
