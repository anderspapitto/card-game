-- A small library for interpreting a FreeT computation inside
-- MonadWidget. An `interpret` function must be provided, which
-- describes what widget to draw in order to handle each effect in the
-- Free monad. `interpret` should use `once` to wrap the `Pure` case.

{-# LANGUAGE RecursiveDo #-}

module Game.FreeInterp (once, runInterpret) where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad.Identity
import Control.Monad.Trans.Free

once :: MonadWidget t m => a -> m (Event t a)
once = (<$> getPostBuild) . fmap . const

splitEither :: (Reflex t) => Event t (Either a b) -> (Event t a, Event t b)
splitEither e = (fmapMaybe (firstOf _Left) e, fmapMaybe (firstOf _Right) e)

runInterpret
  :: (MonadWidget t m, Functor f)
  => (FreeT f Identity r -> m (Event t (Either r (FreeT f Identity r))))
  -> FreeT f Identity r
  -> WidgetHost m ()
  -> m ()
runInterpret interpret val endHook = do
  rec (done, step) <- splitEither <$> (dyn interpWidget >>= switchPromptly never)
      interpWidget <- holdDyn val step >>= mapDyn interpret
  performEvent_ $ ffor done (const endHook)
