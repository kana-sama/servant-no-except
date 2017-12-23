{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

import Universum

import Control.Exception (throwIO, try)

import Data.Aeson (Options (..), ToJSON (..), defaultOptions, genericToJSON)
import Data.Char (toLower)

import Servant ((:<|>) (..), (:>), (:~>) (..), Capture, Get, Handler (..), JSON, Server, enter,
                err404, serve)

import qualified Data.Map.Strict as Map (elems, fromList, lookup)
import qualified Network.Wai.Handler.Warp as Warp (run)


-- DataBase

type UserId = Int
type UserAge = Int
type UserName = String

data User = User
    { _userId   :: UserId
    , _userName :: UserName
    , _userAge  :: UserAge
    } deriving (Generic)

instance ToJSON User where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier }
    where
      fieldLabelModifier name =
        let (x:xs) = drop 5 name
        in toLower x:xs

type DataBaseConnection = Map UserId User
type DataBaseT = ReaderT DataBaseConnection

getUsers :: Monad m => DataBaseT m [User]
getUsers = asks Map.elems

getUser :: Monad m => UserId -> DataBaseT m (Maybe User)
getUser userId = asks (Map.lookup userId)


-- API

type GetUsersEndpoint = "users" :> Get '[JSON] [User]
type GetUserEndpoint  = "users" :> Capture "user-id" UserId :> Get '[JSON] User
type API = GetUsersEndpoint :<|> GetUserEndpoint

type App = DataBaseT IO

handleGetUsers :: App [User]
handleGetUsers = getUsers

handleGetUser :: UserId -> App User
handleGetUser id = getUser id >>= \case
    Nothing   -> liftIO $ throwIO err404
    Just user -> pure user

server :: Server API
server = enter convert $ handleGetUsers :<|> handleGetUser

convert :: App :~> Handler
convert = NT (Handler . ExceptT . try . flip runReaderT connection)
  where
    connection = Map.fromList
        [ (1, User 1 "Andrew" 20)
        , (2, User 2 "Kana" 20)
        , (3, User 3 "Aleph" 20)
        , (4, User 4 "Something" 19)
        ]


-- Main

main :: IO ()
main = Warp.run 8080 $ serve (Proxy :: Proxy API) server
