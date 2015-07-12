{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Display where

import           Control.Lens
import           Control.Monad
import           Data.Aeson.TH
import           Data.List

import qualified Game.DataTypes as D
import           Game.DataTypes (PlayerId)

data Ability = Ability { _description :: String}

instance Show Ability where
  show = _description

displayAbility :: D.Ability -> Ability
displayAbility ability = Ability (D._description ability)

data Effect
  = DamageEff D.Damage
  | DrawEff D.Draw
  | GainResourceEff D.GainResource
  deriving (Show)

displayEffect :: D.Effect -> Effect
displayEffect (D.DamageEff damage) = DamageEff damage
displayEffect (D.DrawEff draw) = DrawEff draw
displayEffect (D.GainResourceEff amount) = GainResourceEff amount

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

displayCost :: D.Cost -> Cost
displayCost (D.Cost g m b r a) = Cost g m b r a

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

displayCard :: D.Card -> Card
displayCard (D.Card n c hb h e a i) = Card n (displayCost c) hb h (map displayAbility e) (map displayAbility a) i

data Attribute
  = AbilityAttr Ability
  | CombatAttr Int
  | Tag String
  | Health Int Int
  | HitsBoard
  | DelayedAbilityAttr Ability
  deriving (Show)

displayAttribute :: D.Attribute -> Attribute
displayAttribute (D.AbilityAttr ability) = AbilityAttr (displayAbility ability)
displayAttribute (D.CombatAttr i) = CombatAttr i
displayAttribute (D.Tag s) = Tag s
displayAttribute (D.Health x y) = Health x y
displayAttribute D.HitsBoard = HitsBoard

data Selection
  = ActionFromBasicState
  | WhichPlayer
  | WhichBoardCard [Card]
  | WhichAbility [Ability]
  | WhichHandCard [Card]
  deriving (Show)

displaySelection :: D.Selection -> Selection
displaySelection D.ActionFromBasicState = ActionFromBasicState
displaySelection D.WhichPlayer = WhichPlayer
displaySelection (D.WhichBoardCard cards) = WhichBoardCard (map displayCard cards)
displaySelection (D.WhichAbility abilities) = WhichAbility (map displayAbility abilities)
displaySelection (D.WhichHandCard cards) = WhichHandCard (map displayCard cards)

data Player = Player
  { _resources :: Cost
  , _hand      :: [Card]
  , _deck      :: Int
  , _board     :: [Card]
  , _playerId  :: PlayerId
  } deriving (Show)

displayPlayer :: D.Player -> Player
displayPlayer (D.Player r h d b p) =
  Player (displayCost r) (map displayCard h) (length d) (map displayCard b) p

data Game = Game
  { _active    :: Player
  , _inactive  :: Player
  , _winner    :: Maybe PlayerId
  } deriving (Show)

displayGame :: D.Game -> Game
displayGame (D.Game a i w) = Game (displayPlayer a) (displayPlayer i) w

liftM concat $ mapM (deriveJSON defaultOptions { fieldLabelModifier = drop 4 })
  [ ''Ability
  , ''Cost
  , ''Card
  , ''PlayerId
  , ''Player
  , ''Selection
  , ''Game
  ]

liftM concat $ mapM makeLenses
  [ ''Ability
  , ''Card
  , ''Game
  , ''Player
  , ''Cost
  ]
