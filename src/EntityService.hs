{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- file: EntityService.hs
-- author: Jacob Xie
-- date: 2024/07/17 20:25:26 Wednesday
-- brief:

module EntityService where

import Control.Exception (try)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 (pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Description (Desc)
import Entities (User)
import Persist
import Servant

type UserAPI =
  "welcome" :> Summary "welcome" :> Get '[PlainText] String
    :<|> "users"
      :> Summary "retrieve all users"
      :> QueryParam' '[Optional, Desc Int "max number of records to load"] "maxRecords" Int
      :> Get '[JSON] [User]
    :<|> "users"
      :> Summary "retrieve user identified by :id"
      :> Capture' '[Desc Id "unique identifier"] ":id" Id
      :> Get '[JSON] User
    :<|> "users"
      :> Summary "store a new user"
      :> ReqBody '[JSON] User
      :> Post '[JSON] ()
    :<|> "users"
      :> Summary "update existing user"
      :> Capture' '[Desc Id "unique identifier"] ":id" Id
      :> ReqBody '[JSON] User
      :> Put '[JSON] ()
    :<|> "users"
      :> Summary "delete existing user"
      :> Capture' '[Desc Id "unique identifier"] ":id" Id
      :> Delete '[JSON] ()

userServer :: Server UserAPI
userServer =
  getWelcome -- GET /welcome
    :<|> getAllUsers -- GET /users
    :<|> getUser -- GET /users/{id}
    :<|> postUser -- POST /users
    :<|> putUser -- POST /users/{id}
    :<|> deleteUser -- DELETE /users/{id}

getWelcome :: Handler String
getWelcome =
  liftIO getCurrentTime
    >>= return . formatTime defaultTimeLocale "%FT%T%QZ"

getAllUsers :: Maybe Int -> Handler [User]
getAllUsers m = do
  liftIO $ putStrLn "GET /users"
  eitherUsersEx <- liftIO $ try (retrieveAll m)
  case eitherUsersEx of
    Left ex -> throwAsServerError ex
    Right l -> return l

getUser :: Id -> Handler User
getUser i = do
  liftIO $ putStrLn $ "GET /users/" <> show i
  eitherUserEx <- liftIO $ try (retrieve i)
  case eitherUserEx of
    Left ex -> throwAsServerError ex
    Right u -> return u

postUser :: User -> Handler ()
postUser user = do
  liftIO $ putStrLn $ "POST /users/" <> show user
  eitherVoidEx <- liftIO $ try (liftIO $ post user)
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v

putUser :: Id -> User -> Handler ()
putUser i user = do
  liftIO $ putStrLn $ "PUT /users/" <> show i <> " " <> show user
  eitherVoidEx <- liftIO $ try (put i user)
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v

deleteUser :: Id -> Handler ()
deleteUser i = do
  liftIO $ putStrLn $ "DELETE /users/" <> show i
  eitherVoidEx <- liftIO $ try (delete userType i)
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v
  where
    userType = Proxy :: Proxy User

----------------------------------------------------------------------------------------------------

throwAsServerError :: (MonadError ServerError m) => PersistErr -> m a
throwAsServerError ex =
  throwError $ case ex of
    EntityNotFound msg -> err404 {errBody = pack msg}
    EntityAlreadyExists msg -> err409 {errBody = pack msg}
    InternalError msg -> err500 {errBody = pack msg}

userAPI :: Proxy UserAPI
userAPI = Proxy
