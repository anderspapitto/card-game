{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Map (toList, fromList)
import           Data.Maybe (isJust)
import           Data.Monoid
import           Network.HTTP
import           Reflex
import           Reflex.Dom hiding (attributes)
import           System.Random
import           Text.Read (readMaybe)

import           Game.ApiServer
import           Game.Cards
import           Game.DataTypes
import qualified Game.Display as Display
import           Game.FreeInterp
import           Game.Logic

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

initial_game :: Interaction Identity ()
initial_game = void $ execStateT runGame initial_game_state

listChoice :: MonadWidget t m => [String] -> m (Event t Int)
listChoice = fmap leftmost . mapM button' . zip [0..]
  where button' (i, s) = fmap (const i) <$> button s

displayGame :: MonadWidget t m => Display.Game -> m ()
displayGame g = do
  el "br" $ text ""
  elAttr "div" (fromList [("style", "width: 100%; display: table;")]) $ do
    elAttr "div" (fromList [("style", "display: table-row;")]) $ do
      elAttr "div" (fromList [("style", "display: table-cell; width; 50%;")]) $ do
        text "Active Player"
        displayPlayer (g ^. Display.active)
      elAttr "div" (fromList [("style", "display: table-cell; width; 50%;")]) $ do
        text "Inactive Player"
        displayPlayer (g ^. Display.inactive)
  return ()

displayResources :: MonadWidget t m => Display.Cost -> m ()
displayResources rr = do
  text (rr ^. Display.gold     . to show . to (" " ++)) >> goldCoin
  text (rr ^. Display.mana     . to show . to (" " ++)) >> flame
  text (rr ^. Display.belief   . to show . to (" " ++)) >> pray
  text (rr ^. Display.research . to show . to (" " ++)) >> brain
  text (rr ^. Display.actions  . to show . to (" " ++)) >> clock

displayCardList :: MonadWidget t m => [Display.Card] -> m ()
displayCardList h = void $ el "div" $ forM_ h (\c ->
  elAttr "img" ("height" =: "50" <>
                "width"  =: "50" <>
                "src"    =: ("/home/anders/devel/card-game/images/by-canon-name/" ++ (c ^. Display.img))) $ text "")

displayPlayer :: MonadWidget t m => Display.Player -> m ()
displayPlayer player = el "div" $ do
  el "div" $ do
    displayResources (player ^. Display.resources)
  el "div" $ do
    text "Hand: "
    displayCardList (player ^. Display.hand)
  el "div" $ do
    text "Cards in Deck: "
    text $ player ^. Display.deck . to show
  el "div" $ do
    text "Board: "
    displayCardList (player ^. Display.board)

bar :: MonadWidget t m => (Maybe Int) -> Workflow t m (Maybe Int)
bar mi = Workflow $ do
  liftIO $ runEitherT (sendInput mi)
  Right foo <- liftIO $ runEitherT getGameState
  case foo of
    Just (g, s) -> do
      foo <- fmap Just <$> (listChoice s <* displayGame g)
      return (mi, fmap bar foo)
    Nothing -> return (Nothing, never)

-- interpreter
--   :: MonadWidget t m
--   => Interaction Identity ()
--   -> m (Event t (Either () (Interaction Identity ())))
-- interpreter p = case runIdentity (runFreeT p) of
--   Free (GetUserChoice s g k) -> fmap (Right . k) <$> (listChoice s <* displayGame g)
--   Free (LogMessage s)        -> undefined
--   Pure ()                    -> once (Left ())

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

fooMain :: IO ()
fooMain = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Card Game"
  workflowView (bar Nothing)
  return ()

main :: IO ()
main = do
  forkIO $ apiMain initial_game
  fooMain
