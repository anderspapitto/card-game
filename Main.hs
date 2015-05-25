{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Read (readMaybe)
import System.Random

import Reflex
import Reflex.Dom hiding (attributes)

-- Datatypes
--

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

type GameM a = StateT Game IO a

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

-- Generic helpers for datatypes
--

gold :: Int -> Cost
gold x = Cost x 0 0 0

mana :: Int -> Cost
mana x = Cost 0 x 0 0

-- Individual actions that will be somehow tagged. They can be
-- manipulated, and they are combined into opaque GameM actions which
-- are "Abilities"
--

chooseTarget :: GameM Int
chooseTarget = do
    b <- use $ player1 . board
    i <- getInput b
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

-- Abilities, opaque GameM actions
--

damageTarget :: Int -> Ability
damageTarget x = Ability ("Deal " ++ show x ++ " damage") go where
  go = chooseTarget >>= damage x

draw :: Int -> Ability
draw x = Ability ("Draw " ++ show x ++ " cards") go where
  go = runDraw player1 3

gain :: Cost -> Ability
gain x = Ability ("Gain " ++ show x ++ " of something") go where
  go = player1 . resources %= (<> x)

-- Game control flow/logic
--

runDraw :: Lens' Game Player -> Int -> GameM ()
runDraw player n = do
  de <- use $ player . deck
  if length de < n
    then error "loss due to deck exhaustion"
    else zoom player $ do
      hand %= (++ take n de)
      deck %= drop n

runGame :: GameM ()
runGame = do
  runDraw player1 5
  runDraw player2 5
  forever (runTurn >> switchTurns)

switchTurns :: GameM ()
switchTurns = do
  lift $ putStrLn "*** Switching Active Player ***"
  modify (\(Game a b) -> Game b a)
  player1 . actions .= 4

getInput :: Show a => [a] -> GameM Int
getInput xs = do
  ret <- lift $ do
    putStrLn "Please select an option"
    putStrLn "0 ** No choice, display game state **"
    forM_ (zip [(1::Int)..] xs) $ \(i, x) -> putStrLn ((show i) ++ ". " ++ show x)
    fmap readMaybe getLine
  case ret of
   Just 0 -> do
     g <- get
     lift $ print g
     getInput xs
   Just n -> return (n - 1)
   Nothing -> do
     lift $ putStrLn "Invalid choice"
     getInput xs

chooseCardInHand :: GameM Card
chooseCardInHand = do
  h <- use $ player1 . hand
  n <- getInput h
  let ret = h !! n
  player1 . hand %= (\l -> take n l ++ drop (n+1) l)
  return ret

resolveCard :: Card -> GameM ()
resolveCard card = do
  paid <- pay $ card ^. cost
  forM_ (card ^. attributes) $ \attr -> case attr of
    AbilityAttr (Ability _ ability) -> ability
    HitsBoard -> player1 . board %= ([card] ++)
    _ -> return ()

chooseAction :: GameM PlayerAction
chooseAction = let options = [PlayCard, DrawCard, GainCoin] in do
  n <- getInput options
  return $ options !! n

playerAction :: GameM ()
playerAction = do
  action <- chooseAction
  case action of
   PlayCard -> playCard
   DrawCard -> runDraw player1 1
   GainCoin -> player1 . resources %= (<> (Cost 1 0 0 0))

playCard :: GameM ()
playCard = do
  card <- chooseCardInHand
  resolveCard card

runTurn :: GameM ()
runTurn = do
  a <- use $ player1 . actions
  when (a > 0) $ do
    player1 . actions -= 1
    playerAction
    runTurn

-- Card definitions
--

draw3 :: Card
draw3 = Card
        "draw3"
        (3 `mana`)
        [AbilityAttr (draw 3)]

burn3 :: Card
burn3 = Card
        "burn3"
        (3 `mana`)
        [AbilityAttr (damageTarget 3)]

bear :: Card
bear = Card
        "bear"
        (2 `gold`)
        [ CombatAttr 2
        , HitsBoard ]

getPaid :: Card
getPaid = Card
        "getpaid"
        (0 `gold`)
        [AbilityAttr (gain (3 `gold`))]

-- Testing stuff
--

randPerm :: StdGen -> [a] -> [a]
randPerm   _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)

baseDeck :: [Card]
baseDeck = concat
  [ replicate 10 getPaid
  , replicate 20 bear
  ]

deck1, deck2 :: [Card]
deck1 = randPerm (mkStdGen 5) baseDeck
deck2 = randPerm (mkStdGen 6) baseDeck

game :: Game
game = Game (Player (Cost 0 0 0 0) 100 [] deck1 [] 4) (Player (Cost 0 0 0 0) 100 [] deck2 [] 4)

data SmallGameState = SmallGameState Int deriving (Show, Eq)

main :: IO ()
main = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Card Game"
  el "ul" $ do
    el "li" $ text "Player 1 has some cards"
    el "li" $ text "Player 2 has some cards"
  text "Pick an action:"
  el "ol" $ do
    el "li" $ text "draw a card"
    el "li" $ text "play a card"
  actionSelection <- el "div" $ textInput $ def & textInputConfig_inputType .~ "number"
  inputLog <- foldDyn (:) [] (tag (current $ _textInput_value actionSelection) (textInputGetEnter actionSelection))

  el "div" $ do
    logAsString <- mapDyn show inputLog
    text "Input Log:"
    dynText $ logAsString
  el "div" $ do
    logAsLen <- count (textInputGetEnter actionSelection) >>= mapDyn show
    text "cards in hand:"
    dynText $ logAsLen
