{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Clay (render, putCss)
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Map (toList, fromList)
import           Data.Monoid
import           Data.Text.Encoding
import           Data.Text.Lazy (toStrict)
import           Reflex
import           Reflex.Dom hiding (attributes)

import           Game.ApiServer (apiMain, getGameState, sendInput)
import           Game.Css
import           Game.Cards
import           Game.DataTypes
import           Game.Logic

listChoice :: (MonadWidget t m, Show a) => [a] -> m (Event t a)
listChoice = fmap leftmost . mapM button'
  where button' s = fmap (const s) <$> button (show s)

displayGame :: MonadWidget t m => Game -> m (Event t SelectionResponse)
                                          --(Event t Int, Event t Int), (Event t Int, Event t Int))
displayGame g = do
  el "br" $ return ()
  ret <- elAttr "div" ("style" =: "width: 1600; display: table;") $ do
    elAttr "div" ("style" =: "display: table-row;") $ do
      active <- elAttr "div" ("style" =: "display: table-cell; width; 800;") $ do
        text "Active Player"
        displayPlayer (g ^. active) Active
      inactive <- elAttr "div" ("style" =: "display: table-cell; width; 50%;") $ do
        text "Inactive Player"
        displayPlayer (g ^. inactive) Inactive
      return $ leftmost [active, inactive]
  el "div" $ do
    text "Messages"
    forM_ (g ^. messages) $ \m -> el "div" (text m)
  return ret

displayResources :: MonadWidget t m => Cost -> m ()
displayResources rr = do
  foo (rr ^. gold    ) goldCoin
  foo (rr ^. mana    ) flame
  foo (rr ^. belief  ) pray
  foo (rr ^. research) brain
  foo (rr ^. actions ) clock
 where
   foo i x = elAttr "div" ("style" =: "height: 48") $ do
     replicateM (i `div` 5) (x 40)
     replicateM (i `mod` 5) (x 32)
     return ()

displayCardList :: MonadWidget t m => [Card] -> m (Event t Int)
displayCardList = elAttr "div" ("class" =: "flex") . fmap leftmost . mapM f . zip [0..]
  where f (i, c) = fmap (const i) <$> displayCard c

displayCard :: MonadWidget t m => Card -> m (Event t ())
displayCard c = do
  (e, _) <- el' "div" $ do
    el "t1" $ text (c ^. name)
    elAttr "img" ("class" =: "imgfloat" <> "src"    =: path c) $ return ()
    case c ^. health of
      Nothing -> return ()
      Just (total, current) -> el "t2" $ text (show total ++ " (" ++ show current ++ ")")
    forM_ (c ^. effects) $ \e ->
      el "div" $ text (e ^. description)
    forM_ (c ^. abilities) $ \a ->
      el "div" $ text (a ^. description)
  return $ (domEvent Click) e
  where path c = "/home/anders/devel/card-game/images/by-canon-name/" ++ (c ^. img)

displayPlayer :: MonadWidget t m => Player -> PlayerActiveness -> m (Event t SelectionResponse)
displayPlayer player a = el "div" $ do
  el "div" $ do
    displayResources (player ^. resources)
  hand <- el "p" $ do
    text "Hand: "
    displayCardList (player ^. hand)
  el "p" $ do
    text "Cards in Deck: "
    text $ player ^. deck . to length . to show
  board <- el "div" $ do
    text "Board: "
    displayCardList (player ^. board)
  el "br" $ return ()
  return $ leftmost [fmap (ResponseHandCard a) hand, fmap (ResponseBoardCard a) board]

bar :: MonadWidget t m => (Maybe SelectionResponse) -> Workflow t m (Maybe SelectionResponse)
bar mi = Workflow $ do
  liftIO $ runEitherT (sendInput mi)
  Right foo <- liftIO $ runEitherT getGameState
  case foo of
    Just (g, s) -> do
      foo <- fmap Just <$> smartStuff g s
      return (mi, fmap bar foo)
    Nothing -> return (Nothing, never)
 where smartStuff g s = case s of
         ActionFromBasicState -> do
           x <- listChoice [ResponseDrawCard, ResponseGainCoin]
           y <- displayGame g
           return $ leftmost [x, y]
         WhichAbility abilities -> undefined
         _ -> displayGame g

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

initial_game = undefined

main :: IO ()
main = do
  putCss gameCss
  _ <- forkIO (apiMain initial_game)
  fooMain
