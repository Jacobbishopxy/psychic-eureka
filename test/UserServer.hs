{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- file: UserServer.hs
-- author: Jacob Xie
-- date: 2024/07/30 08:43:40 Tuesday
-- brief:

module Main where

import Control.Lens (mapped, (&), (?~))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    genericParseJSON,
    genericToJSON,
  )
import Data.Swagger
  ( HasDescription (description),
    HasExample (example),
    ToSchema,
    declareNamedSchema,
    defaultSchemaOptions,
    genericDeclareNamedSchema,
  )
import Data.Swagger.Lens (HasSchema (..))
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import PsychicEureka (Id, genId, mockId, mockUTCTime)
import PsychicEureka.Cache (EntityCache (initialize), EntityCacheStore)
import PsychicEureka.Entity (Entity (..), NameEntity (getName))
import PsychicEureka.Service (EntityService)
import PsychicEureka.Swagger
import Servant (Proxy (..), serve)
import Servant.Server (Server)

----------------------------------------------------------------------------------------------------

data UserInput = UserInput
  { _name :: String,
    _email :: String,
    _password :: String
  }
  deriving (Generic)

instance FromJSON UserInput where
  parseJSON = genericParseJSON jsonOptDropLeadingUnderscore

instance ToJSON UserInput where
  toJSON = genericToJSON jsonOptDropLeadingUnderscore

instance ToSchema UserInput where
  declareNamedSchema proxy =
    genericDeclareNamedSchema swaggerScmDropLeadingUnderscore proxy
      & mapped . schema . description ?~ "Used for create/modify a User"
      & mapped . schema . example ?~ toJSON (UserInput "Jacob" "jacob@prod.com" "123456")

data User = User
  { uid :: Id,
    name :: String,
    email :: String,
    password :: String,
    created_time :: UTCTime,
    modified_time :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, EntityCache, EntityService)

instance ToSchema User where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is a User API (tm)"
      & mapped . schema . example ?~ toJSON (User mockId "JacobX" "xy@dev.com" "123456" mockUTCTime mockUTCTime)

----------------------------------------------------------------------------------------------------
-- impl

instance NameEntity UserInput where
  getName = _name

instance NameEntity User where
  getName = name

instance Entity User where
  type EntityInput User = UserInput

  getId = uid

  getCreatedTime = created_time

  getModifiedTime = modified_time

  createFromInput ui = do
    id' <- genId
    t <- getCurrentTime
    return
      User
        { uid = id',
          name = _name ui,
          email = _email ui,
          password = _password ui,
          created_time = t,
          modified_time = t
        }

  modifyFromInput ui u = do
    t <- getCurrentTime
    return
      u
        { name = _name ui,
          email = _email ui,
          password = _password ui,
          modified_time = t
        }

----------------------------------------------------------------------------------------------------

-- "user" swagger
type UserEntityAPI = EntityAPI "user" User

type UserSwaggerAPI = EntitySwaggerAPI "user" User

userServer :: EntityCacheStore User -> Server UserSwaggerAPI
userServer = swaggerServer entityProxy entityApiProxy apiProxy docInfo
  where
    entityProxy = Proxy @"user"
    apiProxy = Proxy @UserEntityAPI
    entityApiProxy = Proxy @User
    docInfo =
      DocInfo
        { docVersion = "0.1",
          docTitle = "UserServer",
          docDescription = "test lib:psychic-eureka",
          docTag = Just ("users", "users operations")
        }

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let prt = 8080 :: Int

  store <- initialize :: IO (EntityCacheStore User)
  -- open browser
  -- _ <- launch prt
  -- printApiInfo (Proxy @UserEntityAPI)

  let app' = serve (Proxy @UserSwaggerAPI) (userServer store)
  putStrLn $ "Starting server on port " <> show prt

  run prt app'
