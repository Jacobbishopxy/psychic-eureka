{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- file: UserServer.hs
-- author: Jacob Xie
-- date: 2024/07/30 08:43:40 Tuesday
-- brief:

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import PsychicEureka (Id, genId)
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
  deriving (Generic, FromJSON, ToSchema)

data User = User
  { uid :: Id,
    name :: String,
    email :: String,
    password :: String,
    created_time :: UTCTime,
    modified_time :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, EntityCache, EntityService)

----------------------------------------------------------------------------------------------------
-- impl

-- instance ToSchema UserInput

instance ToSchema User

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
  _ <- launch prt

  let userType = Proxy :: Proxy User
      entityApi = Proxy :: Proxy (EntityAPI User)
      sd = swaggerDoc entityApi docInfo
      es = entityServer userType store
      -- ss = swaggerServer userType docInfo store :: Server (API User)
      ss = swaggerSchemaUIServer sd :<|> es :: Server (API User)
      -- app' = app (Proxy :: Proxy (EntityAPI User)) docInfo store
      app' = serve (Proxy :: Proxy (API User)) ss

  putStrLn $ "Starting server on port " <> show prt

  run prt app'
