{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.DataTypes where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Data.Aeson
import Data.Aeson.TH
import Data.List
import GHC.Generics
import Servant.Common.Text

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

data Cost = Cost
  { _gold     :: Int
  , _mana     :: Int
  , _belief   :: Int
  , _research :: Int
  , _actions  :: Int
  }

instance Show Cost where
  show (Cost g m b r a) =
    let pairs = [(g, "gold"), (m, "mana"), (b, "belief"), (r, "reserach"), (a, "actions")]
    in intercalate " and " $ map (\(x, name) -> show x ++ " " ++ name) $ filter ((/= 0) . fst) pairs

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
  , _hitsBoard  :: Bool
  , _health     :: Maybe (Int, Int)
  , _effects    :: [Ability]
  , _abilities  :: [Ability]
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
  | DelayedAbilityAttr Ability
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
  , _messages  :: [String]
  } deriving (Show, Generic)

data PlayerActiveness = Active | Inactive deriving (Show, Read)

data Selection
  = ActionFromBasicState
  | WhichPlayer
  | WhichBoardCard PlayerActiveness
  | WhichAbility [Ability]
  | WhichHandCard [Card]
  deriving (Show)

data SelectionResponse
  = ResponseGainCoin
  | ResponseDrawCard
  | ResponseHandCard PlayerActiveness Int
  | ResponseBoardCard PlayerActiveness Int
  deriving (Show, Read)

instance FromText SelectionResponse where
  fromText = fmap read . fromText

instance ToText SelectionResponse where
  toText = toText . show

data InteractionF x
  = GetUserChoice Selection Game (SelectionResponse -> x)

instance Functor InteractionF where
  fmap f (GetUserChoice s g k) = GetUserChoice s g (f . k)

type Interaction m a = FreeT InteractionF m a

type GameM a = StateT Game (FreeT InteractionF Identity) a

instance ToJSON (StateT Game (FreeT InteractionF Identity) a)
  where toJSON = const Null

instance FromJSON (StateT Game (FreeT InteractionF Identity) a)
  where parseJSON = const (return undefined)

getUserChoice :: Selection -> GameM SelectionResponse
getUserChoice options = do
  g <- get
  liftF $ GetUserChoice options g id

data PlayerAction
  = PlayCard
  | DrawCard
  | GainCoin
  | SelectCardOnBoard
  deriving (Show)

liftM concat $ mapM (deriveJSON defaultOptions { fieldLabelModifier = drop 4 })
  [ ''Ability
  , ''PlayerActiveness
  , ''Cost
  , ''Card
  , ''PlayerId
  , ''Player
  , ''Selection
  , ''SelectionResponse
  , ''Game
  ]

liftM concat $ mapM makeLenses
  [ ''Ability
  , ''Card
  , ''Game
  , ''Player
  , ''Cost
  ]
