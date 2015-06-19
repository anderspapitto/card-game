{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Game.Shell
import Game.DataTypes
import Game.Cards
import Game.Logic
import Game.FreeInterp

import Control.Lens
import Control.Monad.Fix
import Text.Read (readMaybe)
import System.Random
import Reflex
import Reflex.Dom hiding (attributes)
import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Data.Map (toList, fromList)
import Data.Maybe (isJust)
import Data.Monoid

randPerm :: StdGen -> [a] -> [a]
randPerm   _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)

buttonifiedList :: MonadWidget t m => Dynamic t [ String ] -> m (Event t Int)
buttonifiedList options = do
  asMap <- mapDyn (fromList . map (\(i,x) -> ((i,x), x)) . zip [(0::Int)..]) options
  foo <- listWithKey asMap go
  let x = (fmap (fst .fst . head . toList) (updated foo))
  return x
 where
   go _ dv = dyn =<< mapDyn button dv

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
initial_game_state = Game (Player (Cost 9 9 9 9 3) [] deck1 [] (PlayerId 0))
                          (Player (Cost 9 9 9 9 3) [] deck2 [] (PlayerId 1))
                          Nothing

initial_game :: Foo Game
initial_game = case runIdentity $ runFreeT $ execStateT runGame initial_game_state of
  Free (GetUserChoice s g f) -> (s, Just g, Just f)
  Pure _                     -> error "not ok"

displayList :: MonadWidget t m => Dynamic t [String] -> m ()
displayList d = do
  asMap <- mapDyn (fromList . zip [(0::Int)..]) d
  void $ el "ol" $ list asMap (el "li" . dynText)

dynButton :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton = fmap (_el_clicked . fst) . el' "button" . dynText

listChoice :: MonadWidget t m => Dynamic t [String] -> m (Event t Int)
listChoice choices = el "div" $ do
  asMap <- mapDyn (fromList . zip [(0::Int)..]) choices
  evs <- listWithKey asMap (const dynButton)
  fmap switchPromptlyDyn $ mapDyn (leftmost . map keyEvent . toList) evs
 where
  keyEvent = uncurry (fmap . const)

options :: MonadWidget t m => Dynamic t Int -> m (Dynamic t [String])
options = mapDyn (\x -> map show [0..(x+2)])

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
  mapDyn (^. gold     . to show . to (" " ++)) rr >>= dynText >> goldCoin
  mapDyn (^. mana     . to show . to (" " ++)) rr >>= dynText >> flame
  mapDyn (^. belief   . to show . to (" " ++)) rr >>= dynText >> pray
  mapDyn (^. research . to show . to (" " ++)) rr >>= dynText >> brain
  mapDyn (^. actions  . to show . to (" " ++)) rr >>= dynText >> clock

displayCardList :: MonadWidget t m => Dynamic t [Card] -> m ()
displayCardList h = do
  void $ elDynHtml' "div" =<< do
    forDyn h $ \cs -> concat (map (\c -> "<img height=50 width=50 src=/home/anders/devel/card-game/images/by-canon-name/" ++ (c ^. img) ++ ">") cs)

displayPlayer :: MonadWidget t m => Dynamic t Player -> m ()
displayPlayer player = el "div" $ do
  el "div" $ do
    displayResources =<< mapDyn (^. resources) player
  el "div" $ do
    text "Hand: "
    displayCardList =<< mapDyn (^. hand) player
  el "div" $ do
    text "Cards in Deck: "
    dynText =<< mapDyn (show . length . (^. deck)) player
  el "div" $ do
    text "Board: "
    displayCardList =<< mapDyn (^. board) player

main :: IO ()
main = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Card Game"
  rec
    selections   <- listChoice prompts
    _            <- displayGame d
    game         <- stepGame selections
    (prompts, d) <- mapDyn (\(p, g, _) -> (p, g)) game >>= splitDyn
  return ()

makeImage :: MonadWidget t m => String -> Int -> m ()
makeImage path size = elAttr "img"
                      ("src" =: ("/home/anders/devel/card-game/images/by-canon-name/" ++ path)
                       <> "width" =: (show size) <> "height" =: (show size))
                      (return ())

goldCoin :: MonadWidget t m => m ()
goldCoin = makeImage "gold-coin" 32

flame :: MonadWidget t m => m ()
flame = makeImage "flame" 32

pray :: MonadWidget t m => m ()
pray = makeImage "pray" 32

brain :: MonadWidget t m => m ()
brain = makeImage "brain" 32

clock :: MonadWidget t m => m ()
clock = makeImage "clock" 32
