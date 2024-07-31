{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- file: TypeF.hs
-- author: Jacob Xie
-- date: 2024/07/27 16:22:57 Saturday
-- brief:

module PsychicEureka.Swagger.TypeF
  ( Desc,
    EntityAPI,
    EntitySwaggerAPI,
    printApiInfo,
  )
where

import Data.Data (Typeable, typeRep)
import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (AppendSymbol, Symbol)
import PsychicEureka (Id)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import Servant
import Servant.Swagger.UI (SwaggerSchemaUI)

----------------------------------------------------------------------------------------------------
-- Desc

type family TypeName (x :: Type) :: Symbol where
  TypeName Int = "integer"
  TypeName String = "string"
  TypeName x = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: Type) :: Symbol where
  GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))

----------------------------------------------------------------------------------------------------
-- API

-- | Type family to concatenate two type-level symbols.
type family (:<>:) (s1 :: Symbol) (s2 :: Symbol) :: Symbol where
  s1 :<>: s2 = AppendSymbol s1 s2

-- | Type family to define the API for an entity.
-- The API includes various endpoints for CRUD operations and more.
type family EntityAPI (name :: Symbol) a where
  EntityAPI name a =
    (name :<>: "_entity_info")
      :> Summary ("get entity name and the current time")
      :> Get '[PlainText] String
      :<|> (name :<>: "_name_map")
        :> Summary (name :<>: " name id mapping")
        :> Get '[JSON] Cache.NameIdMapping
      :<|> (name :<>: "_id_by_name")
        :> Summary ("retrieve " :<>: name :<>: " id by name")
        :> QueryParam' '[Required, Desc String (name :<>: " name")] "name" String
        :> Get '[JSON] Id
      :<|> name
        :> Summary ("retrieve " :<>: name :<>: " identified by :id")
        :> Capture' '[Desc Id "unique identifier"] ":id" Id
        :> Get '[JSON] a
      :<|> (name :<>: "_by_name")
        :> Summary ("retrieve " :<>: name :<>: " by name")
        :> QueryParam' '[Required, Desc String (name :<>: " name")] "name" String
        :> Get '[JSON] a
      :<|> (name :<>: "_all")
        :> Summary ("retrieve all " :<>: name)
        :> Get '[JSON] [a]
      :<|> name
        :> Summary ("store a new " :<>: name)
        :> ReqBody '[JSON] (Entity.EntityInput a)
        :> Post '[JSON] a
      :<|> name
        :> Summary ("modify an existing " :<>: name)
        :> Capture' '[Desc Id "unique identifier"] ":id" Id
        :> ReqBody '[JSON] (Entity.EntityInput a)
        :> Put '[JSON] a
      :<|> (name :<>: "_by_name")
        :> Summary ("modify an existing " :<>: name :<>: " by name")
        :> QueryParam' '[Required, Desc String (name :<>: " name")] "name" String
        :> ReqBody '[JSON] (Entity.EntityInput a)
        :> Put '[JSON] a
      :<|> name
        :> Summary ("delete an existing " :<>: name)
        :> Capture' '[Desc Id "unique identifier"] ":id" Id
        :> Delete '[JSON] a
      :<|> (name :<>: "_by_name")
        :> Summary ("delete an existing " :<>: name :<>: " by name")
        :> QueryParam' '[Required, Desc String (name :<>: " name")] "name" String
        :> Delete '[JSON] a

-- | Type family to define the combined Swagger and entity API.
-- This includes the Swagger UI endpoint and the 'EntityAPI' endpoints.
type family EntitySwaggerAPI (name :: Symbol) a where
  EntitySwaggerAPI name a = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> EntityAPI name a

----------------------------------------------------------------------------------------------------

-- | 'printApiInfo' prints the API prefix and the type of the API to the console.
printApiInfo :: forall api. (Typeable api) => Proxy api -> IO ()
printApiInfo _ = do
  putStrLn $ "Generated API type:\n" ++ show (typeRep (Proxy @api))
