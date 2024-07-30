{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- file: UserServer.hs
-- author: Jacob Xie
-- date: 2024/07/30 08:43:40 Tuesday
-- brief:

module Main where

import Control.Lens (mapped, (&), (?~))
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, withObject, (.:))
import Data.Swagger (HasDescription (description), HasExample (example), SchemaOptions (..), ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier, genericDeclareNamedSchema)
import Data.Swagger.Lens (HasSchema (..))
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import PsychicEureka (Id, genId, mockId, mockUTCTime)
import PsychicEureka.Cache (EntityCache (initialize), EntityCacheStore)
import PsychicEureka.Entity (Entity (..), NameEntity (getName))
import PsychicEureka.Service (EntityService)
import PsychicEureka.Swagger
import Servant (Proxy (..), Server, serve, type (:<|>) ((:<|>)))
import Servant.Swagger.UI (swaggerSchemaUIServer)

----------------------------------------------------------------------------------------------------

data UserInput = UserInput
  { name_ :: String,
    email_ :: String,
    password_ :: String
  }
  deriving (Generic)

instance FromJSON UserInput where
  parseJSON = withObject "UserInput" $ \v ->
    UserInput <$> v .: "name" <*> v .: "email" <*> v .: "password"

instance ToJSON UserInput where
  toJSON (UserInput n e p) =
    object ["name" .= n, "email" .= e, "password" .= p]

instance ToSchema UserInput where
  declareNamedSchema proxy =
    genericDeclareNamedSchema schemaOptions proxy
      & mapped . schema . description ?~ "Used for create/modify a User"
      & mapped . schema . example ?~ toJSON (UserInput "Jacob" "jacob@prod.com" "123456")
    where
      schemaOptions =
        defaultSchemaOptions
          { fieldLabelModifier = dropTrailingUnderscore
          }

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore = reverse . dropWhile (== '_') . reverse

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
  getName = name_

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
          name = name_ ui,
          email = email_ ui,
          password = password_ ui,
          created_time = t,
          modified_time = t
        }

  modifyFromInput ui u = do
    t <- getCurrentTime
    return
      u
        { name = name_ ui,
          email = email_ ui,
          password = password_ ui,
          modified_time = t
        }

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let prt = 8080 :: Int
      docInfo = DocInfo {docVersion = "0.1", docTitle = "UserServer", docDescription = "test lib:psychic-eureka"}

  store <- initialize :: IO (EntityCacheStore User)
  -- _ <- launch prt

  let userType = Proxy :: Proxy User
      entityApi = Proxy :: Proxy (EntityAPI User)
      sd = swaggerDoc entityApi docInfo
      es = entityServer userType store
      ss = swaggerSchemaUIServer sd :<|> es :: Server (API User)
      app' = serve (Proxy :: Proxy (API User)) ss

  putStrLn $ "Starting server on port " <> show prt

  run prt app'
