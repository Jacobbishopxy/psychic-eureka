{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Data
import Data.Swagger
import GHC.Generics (Generic)
import Servant
import Servant.Swagger

data User = User {name :: String, age :: Int} deriving (Show, Generic, Typeable)

newtype UserId = UserId Integer deriving (Show, Generic, Typeable, ToJSON)

instance ToJSON User

instance ToSchema User

instance ToSchema UserId

instance ToParamSchema UserId

type GetUsers = Get '[JSON] [User]

type GetUser = Capture "user_id" UserId :> Get '[JSON] User

type PostUser = ReqBody '[JSON] User :> Post '[JSON] UserId

type UserAPI = GetUsers :<|> GetUser :<|> PostUser

-- Generate Swagger documentation
swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy UserAPI)
    & info . title .~ "User API"
    & info . version .~ "1.0"
    & applyTagsFor
      (subOperations (Proxy :: Proxy (GetUsers :<|> GetUser)) (Proxy :: Proxy UserAPI))
      ["get" & description ?~ "GET operations"]
    & applyTagsFor
      (subOperations (Proxy :: Proxy PostUser) (Proxy :: Proxy UserAPI))
      ["post" & description ?~ "POST operations"]

-- Function to print Swagger object
printSwagger :: Swagger -> IO ()
printSwagger swagger = BSL8.putStrLn $ encodePretty swagger

main :: IO ()
main = do
  printSwagger swaggerDoc
