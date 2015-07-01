{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.DataTypes where

import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

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
  { _rname  :: String
  , _amount :: Integer
  , _upkeep :: UpkeepStrategy
  }

data UpkeepStrategy = Reset | Keep | Subtract Integer

-- resources = [ gold, mana, belief, research ]
--   where gold = Resource "gold" 0 Keep
--         mana = Resource "mana" 0 Reset
--         belief = Resource "belief" 0 (Subtract 2)
--         research = Resource "research" 0 Keep

data Cost = Cost
  { _gold     :: Int
  , _mana     :: Int
  , _belief   :: Int
  , _research :: Int
  , _actions  :: Int
  } deriving (Show)

instance Monoid Cost where
  mempty = Cost 0 0 0 0 0
  Cost a1 b1 c1 d1 e1 `mappend` Cost a2 b2 c2 d2 e2 =
    Cost (max 0 (a1 + a2))
         (max 0 (b1 + b2))
         (max 0 (c1 + c2))
         (max 0 (d1 + d2))
         (max 0 (e1 + e2))

data Card = Card
  { _name       :: String
  , _cost       :: Cost
  , _attributes :: [ Attribute ]
  , _img        :: String
  }

instance Show Card where
  show = _name

data Attribute
  = AbilityAttr Ability
  | CombatAttr Int
  | Tag String
  | Health Int Int -- | total and current (minus damage)
  | HitsBoard
  deriving (Show)

newtype PlayerId = PlayerId { _getPlayerId :: Int } deriving (Eq, Show)

data Player = Player
  { _resources :: Cost
  , _hand      :: [Card]
  , _deck      :: [Card]
  , _board     :: [Card]
  , _playerId  :: PlayerId
  } deriving (Show)

data Game = Game
  { _active    :: Player
  , _inactive  :: Player
  , _winner    :: Maybe PlayerId
  } deriving (Show, Generic)

data InteractionF x
  = GetUserChoice [String] Game (Int -> x)
  | LogMessage String

instance Functor InteractionF where
  fmap f (GetUserChoice s g k) = GetUserChoice s g (f . k)
  fmap f (LogMessage s) = LogMessage s

type Interaction m a = FreeT InteractionF m a

type GameM a = StateT Game (FreeT InteractionF Identity) a

getUserChoice :: Show a => [a] -> GameM Int
getUserChoice options = do
  g <- get
  ret <- liftF $ GetUserChoice (map show options) g id
  if ret < length options && ret >= 0
    then return ret
    else getUserChoice options

logMessage :: String -> GameM ()
logMessage = liftF . LogMessage

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
makeLenses ''Cost
