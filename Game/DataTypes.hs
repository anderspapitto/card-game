{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.DataTypes where

import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Free

type Activate = Cost
data Damage = Damage Int () deriving (Show)
type Draw = Int
data GainResource = GainResource Cost deriving Show

data Ability = Ability
  { _description :: String
  , _runAbility  :: GameM ()
  }

instance Show Ability where
  show = _description

data Effect
  = DamageEff Damage
  | DrawEff Draw
  | GainResourceEff GainResource
  deriving (Show)

data Resource = Resource
  { _rname   :: String
  , _amount :: Integer
  , _upkeep :: UpkeepStrategy
  }

data UpkeepStrategy = Reset | Keep | Subtract Integer

-- resources = [ gold, mana, belief, research ]
--   where gold = Resource "gold" 0 Keep
--         mana = Resource "mana" 0 Reset
--         belief = Resource "belief" 0 (Subtract 2)
--         research = Resource "research" 0 Keep

data Cost = Cost Int Int Int Int deriving (Show)

instance Monoid Cost where
  mempty = Cost 0 0 0 0
  Cost a1 b1 c1 d1 `mappend` Cost a2 b2 c2 d2 =
    Cost (max 0 (a1 + a2))
         (max 0 (b1 + b2))
         (max 0 (c1 + c2))
         (max 0 (d1 + d2))

data Card = Card
  { _name       :: String
  , _cost       :: Cost
  , _attributes :: [ Attribute ]
  }

instance Show Card where
  show = _name

data Attribute
  = AbilityAttr Ability
  | CombatAttr Int
  | Tag String
  | HitsBoard
  deriving (Show)

data Player = Player
  { _resources :: Cost
  , _health    :: Int
  , _hand      :: [Card]
  , _deck      :: [Card]
  , _board     :: [Card]
  , _actions   :: Int
  } deriving (Show)

data Game = Game
  { _player1 :: Player
  , _player2 :: Player
  } deriving (Show)

data InteractionF x
  = GetUserChoice String (Int -> x)

instance Functor InteractionF where
  fmap f (GetUserChoice s k) = GetUserChoice s (f . k)

type Interaction m a = FreeT InteractionF m a

getUserChoice :: (MonadFree InteractionF m, Show a) => [a] -> m Int
getUserChoice options = liftF $ GetUserChoice (show options) id

type GameM a = StateT Game (FreeT InteractionF Identity) a

data PlayerAction
  = PlayCard
  | DrawCard
  | GainCoin
  deriving (Show)

-- Derive lenses
--

makeLenses ''Card
makeLenses ''Game
makeLenses ''Player

gold :: Int -> Cost
gold x = Cost x 0 0 0

mana :: Int -> Cost
mana x = Cost 0 x 0 0
