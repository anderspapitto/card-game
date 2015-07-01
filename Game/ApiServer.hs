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
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           GHC.Generics
import           Game.DataTypes
import qualified Game.Display as Display
import           Network.HTTP
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client

type GameApi
  =    "game"                          :> Get  '[JSON] (Maybe (Display.Game, [String]))
  :<|> "input" :> QueryParam "inp" Int :> Post '[JSON] ()

serveGame
  :: MVar (Interaction Identity ())
  -> EitherT ServantErr IO (Maybe (Display.Game, [String]))
serveGame game = do
  p <- liftIO $ readMVar game
  return $ case runIdentity $ runFreeT p of
    Free (GetUserChoice s g _) -> Just (Display.displayGame g, s)
    Free (LogMessage s)        -> undefined
    Pure ()                    -> Nothing

serveInput
  :: MVar (Interaction Identity ())
  -> Maybe Int
  -> EitherT ServantErr IO ()
serveInput game i = case i of
  Nothing -> return ()
  Just i -> liftIO $ modifyMVar_ game $ \p -> case runIdentity $ runFreeT p of
    Free (GetUserChoice _ _ k) -> return $ k i
    Free (LogMessage s)        -> undefined
    Pure ()                    -> undefined

server :: MVar (Interaction Identity ()) -> Server GameApi
server game = (serveGame game) :<|> (serveInput game)

gameAPI :: Proxy GameApi
gameAPI = Proxy

app :: MVar (Interaction Identity ()) -> Application
app game = serve gameAPI (server game)

getGameState :<|> sendInput = client gameAPI (BaseUrl Http "localhost" 8081)

apiMain :: (Interaction Identity ()) -> IO ()
apiMain initial_game = do
  game <- newEmptyMVar
  putMVar game initial_game
  run 8081 $ app game
