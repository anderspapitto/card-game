{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Game.ApiServer where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Free
import           Game.DataTypes
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client

type GameApi
  =    "game"                                        :> Get  '[JSON] (Maybe (Game, Selection))
  :<|> "input" :> QueryParam "inp" SelectionResponse :> Post '[JSON] ()

serveGame
  :: MVar (Interaction Identity ())
  -> EitherT ServantErr IO (Maybe (Game, Selection))
serveGame game = do
  p <- liftIO $ readMVar game
  return $ case runIdentity $ runFreeT p of
    Free (GetUserChoice s g _) -> Just (g, s)
    Pure ()                    -> Nothing

serveInput
  :: MVar (Interaction Identity ())
  -> Maybe SelectionResponse
  -> EitherT ServantErr IO ()
serveInput game mi = case mi of
  Nothing -> return ()
  Just sr -> liftIO $ modifyMVar_ game $ \p -> case runIdentity $ runFreeT p of
    Free (GetUserChoice _ _ k) -> return $ k sr
    Pure ()                    -> error "bad there"

server :: MVar (Interaction Identity ()) -> Server GameApi
server game = (serveGame game) :<|> (serveInput game)

gameAPI :: Proxy GameApi
gameAPI = Proxy

app :: MVar (Interaction Identity ()) -> Application
app game = serve gameAPI (server game)

getGameState :: EitherT ServantError IO (Maybe (Game, Selection))
sendInput :: Maybe SelectionResponse -> EitherT ServantError IO ()

getGameState :<|> sendInput = client gameAPI (BaseUrl Http "localhost" 8081)

apiMain :: (Interaction Identity ()) -> IO ()
apiMain initial_game = do
  game <- newEmptyMVar
  putMVar game initial_game
  run 8081 $ app game
