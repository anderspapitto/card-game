{-# LANGUAGE RecursiveDo #-}

module Main where

import Game.Shell
import Game.DataTypes
import Game.Cards
import Game.Logic

import Control.Lens
import Control.Monad.Fix
import Text.Read (readMaybe)
import System.Random
import Reflex
import Reflex.Dom hiding (attributes)
import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Data.Map (fromList)
import Data.Maybe (isJust)

randPerm :: StdGen -> [a] -> [a]
randPerm   _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)

baseDeck :: [Card]
baseDeck = concat
  [ replicate 10 getPaid
  , replicate 10 bear
  , replicate 10 burn3
  , replicate 10 draw3
  ]

deck1, deck2 :: [Card]
deck1 = randPerm (mkStdGen 5) baseDeck
deck2 = randPerm (mkStdGen 6) baseDeck

initial_game_state :: Game
initial_game_state = Game (Player (Cost 9 9 9 9 3) 100 [] deck1 [] (PlayerId 0))
                          (Player (Cost 9 9 9 9 3) 100 [] deck2 [] (PlayerId 1))
                          Nothing

initial_game :: Foo Game
initial_game = case runIdentity $ runFreeT $ execStateT runGame initial_game_state of
  Free (GetUserChoice s g f) -> (s, Just g, Just f)
  Pure _                     -> error "not ok"

displayList :: MonadWidget t m => Dynamic t [String] -> m ()
displayList d = do
  asMap <- mapDyn (fromList . zip [(1::Int)..]) d
  void $ el "ol" $ list asMap (el "li" . dynText)

getSelections :: MonadWidget t m => Dynamic t [String] -> m (Event t Int)
getSelections prompt = do
  -- lstDyn <- holdDyn ["foo"] (updated prompt)
  -- lst <- sample $ current lstDyn
  -- if length lst > 0
  -- then do
    el "div" $ text "Please enter a number corresponding to one of the following options"
    displayList prompt
    inp <- el "div" $ textInput $ def & textInputConfig_inputType .~ "number"
    return $ attachDynWithMaybe (const . (fmap (\x -> x - 1) . readMaybe))
                                (inp ^. textInput_value)
                                (textInputGetEnter inp)
  -- else return never

stepGame :: (Reflex t, MonadHold t m, MonadFix m) => Event t Int -> m (Dynamic t (Foo Game))
stepGame selections = foldDyn step initial_game selections

displayGame :: MonadWidget t m => Dynamic t (Maybe DisplayInfo) -> m ()
displayGame g = do
  mostRecentGame <- let foobar mg og = case mg of
                          Just ng -> ng
                          Nothing -> og
                    in foldDyn foobar (initial_game_state :: Game) (updated g)
  isGame <- mapDyn isJust g
  doShowDyn <- holdDyn True (updated isGame)
  doShow <- sample $ current doShowDyn
  when doShow $ do
    el "br" $ text ""
    elAttr "div" (fromList [("style", "width: 100%; display: table;")]) $ do
      elAttr "div" (fromList [("style", "display: table-row;")]) $ do
        elAttr "div" (fromList [("style", "display: table-cell; width; 50%;")]) $ do
          text "Active Player"
          displayPlayer =<< mapDyn (^. active) mostRecentGame
        elAttr "div" (fromList [("style", "display: table-cell; width; 50%;")]) $ do
          text "Inactive Player"
          displayPlayer =<< mapDyn (^. inactive) mostRecentGame
  return ()

displayResources :: MonadWidget t m => Dynamic t Cost -> m ()
displayResources rr = do
  displayList =<< mapDyn asPieces rr
  where
    asPieces r = [ "Gold: " ++ show (r ^. gold)
                 , "Mana: " ++ show (r ^. mana)
                 , "Belief: " ++ show (r ^. belief)
                 , "Research: " ++ show (r ^. research)
                 , "Actions: " ++ show (r ^. actions)
                 ]

displayPlayer :: MonadWidget t m => Dynamic t Player -> m ()
displayPlayer player = el "div" $ do
  el "div" $ do
    text "Health: "
    dynText =<< mapDyn (show . (^. health)) player
  el "resources" $ do
    text "Resources: "
    displayResources =<< mapDyn (^. resources) player
  el "div" $ do
    text "Cards in Hand: "
    displayList =<< mapDyn (map show . (^. hand)) player
  el "div" $ do
    text "Cards in Deck: "
    dynText =<< mapDyn (show . length . (^. deck)) player
  el "div" $ do
    text "Cards on Board: "
    displayList =<< mapDyn (map show . (^. board)) player

main :: IO ()
main = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Card Game"
  rec
    selections   <- getSelections prompts
    _            <- displayGame d
    game         <- stepGame selections
    (prompts, d) <- mapDyn (\(p, g, _) -> (p, g)) game >>= splitDyn
  return ()
