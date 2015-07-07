{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Clay (render, putCss)
import           Control.Concurrent
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.Map (toList, fromList)
import           Data.Monoid
import           Data.Text.Encoding
import           Data.Text.Lazy (toStrict)
import           Reflex
import           Reflex.Dom hiding (attributes)
import           System.Random

import           Game.ApiServer
import           Game.Css
import           Game.Cards
import           Game.DataTypes
import qualified Game.Display as Display
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
  , replicate 5 base
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
  elAttr "div" ("style" =: "width: 100%; display: table;") $ do
    elAttr "div" ("style" =: "display: table-row;") $ do
      elAttr "div" ("style" =: "display: table-cell; width; 50%;") $ do
        text "Active Player"
        displayPlayer (g ^. Display.active)
      elAttr "div" ("style" =: "display: table-cell; width; 50%;") $ do
        text "Inactive Player"
        displayPlayer (g ^. Display.inactive)
  return ()

displayResources :: MonadWidget t m => Display.Cost -> m ()
displayResources rr = do
  foo (rr ^. Display.gold    ) goldCoin
  foo (rr ^. Display.mana    ) flame
  foo (rr ^. Display.belief  ) pray
  foo (rr ^. Display.research) brain
  foo (rr ^. Display.actions ) clock
 where
   foo i x = elAttr "div" ("style" =: "height: 48") $ do
     replicateM (i `div` 5) (x 40)
     replicateM (i `mod` 5) (x 32)
     return ()

displayCardList :: MonadWidget t m => [Display.Card] -> m ()
displayCardList h = do
  void $ elAttr "div" ("class" =: "flex") $ mapM_ displayCard h

displayCard :: MonadWidget t m => Display.Card -> m ()
displayCard c =
    el "div" $ do
      el "t1" $ text (c ^. Display.name)
      elAttr "img" ("class" =: "imgfloat" <> "src"    =: path c) $ text ""
      case c ^. Display.health of
        Nothing -> return ()
        Just (total, current) -> el "t2" $ text (show total ++ " (" ++ show current ++ ")")
      forM_ (c ^. Display.effects) $ \e ->
        el "div" $ text (e ^. Display.description)
      forM_ (c ^. Display.abilities) $ \a ->
        el "div" $ text (a ^. Display.description)
 where path c = "/home/anders/devel/card-game/images/by-canon-name/" ++ (c ^. Display.img)

displayPlayer :: MonadWidget t m => Display.Player -> m ()
displayPlayer player = el "div" $ do
  el "div" $ do
    displayResources (player ^. Display.resources)
  el "p" $ do
    text "Hand: "
    displayCardList (player ^. Display.hand)
  el "p" $ do
    text "Cards in Deck: "
    text $ player ^. Display.deck . to show
  el "div" $ do
    text "Board: "
    displayCardList (player ^. Display.board)
  el "br" $ text ""

bar :: MonadWidget t m => (Maybe Int) -> Workflow t m (Maybe Int)
bar mi = Workflow $ do
  liftIO $ runEitherT (sendInput mi)
  Right foo <- liftIO $ runEitherT getGameState
  case foo of
    Just (g, s) -> do
      foo <- fmap Just <$> (listChoice s <* displayGame g)
      return (mi, fmap bar foo)
    Nothing -> return (Nothing, never)

makeImage :: MonadWidget t m => String -> Int -> m ()
makeImage path size = elAttr "img"
                      ("src" =: ("/home/anders/devel/card-game/images/by-canon-name/" ++ path)
                       <> "width" =: (show size) <> "height" =: (show size))
                      (return ())

goldCoin :: MonadWidget t m => Int -> m ()
goldCoin = makeImage "gold-coin"

flame :: MonadWidget t m => Int -> m ()
flame = makeImage "flame"

pray :: MonadWidget t m => Int -> m ()
pray = makeImage "pray"

brain :: MonadWidget t m => Int -> m ()
brain = makeImage "brain"

clock :: MonadWidget t m => Int -> m ()
clock = makeImage "clock"

fooMain :: IO ()
fooMain = mainWidgetWithCss (encodeUtf8 . toStrict $ render gameCss) $ el "div" $ do
  el "p" $ text "Welcome to Card Game"
  workflowView (bar Nothing)
  return ()

main :: IO ()
main = do
  putCss gameCss
  _ <- forkIO (apiMain initial_game)
  fooMain
