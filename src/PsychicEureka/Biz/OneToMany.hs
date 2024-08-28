{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- file: OneToMany.hs
-- author: Jacob Xie
-- date: 2024/08/12 13:42:45 Monday
-- brief:

module PsychicEureka.Biz.OneToMany
  ( CacheOneToMany (..),
    OneToMany (..),
  )
where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (throw)
import Data.Aeson (FromJSON, ToJSON, encodeFile)
import Data.Data (Proxy (..), typeRep)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import PsychicEureka.Error (EurekaError (..))
import qualified PsychicEureka.Internal.Util as Util
import PsychicEureka.Util (Id)

----------------------------------------------------------------------------------------------------
-- Types & Newtype
----------------------------------------------------------------------------------------------------

-- | The ID of the main entity in the one-to-many relationship.
type MainId = Id

-- | The ID of the reference entity in the one-to-many relationship.
type RefId = Id

-- | The name of the main entity.
type MainName = String

-- | The name of the reference entity.
type RefName = String

-- | A map representing the one-to-many relationship, where a main entity ID maps to a list of reference entity IDs.
type RefO2M = Map.Map MainId [RefId]

-- | A newtype wrapper for the one-to-many relationship, enabling serialization and deserialization.
newtype RefRelationO2MData = RefRelationO2MData RefO2M
  deriving (Generic, ToJSON, FromJSON)

-- | A mutable reference to the one-to-many relationship data.
type RefRelationO2M = MVar RefRelationO2MData

-- | The cache for a one-to-many relationship, including caches for the main entity, the reference entity,
-- and the reference relationship data.
data CacheOneToMany a b = CacheOneToMany
  { cacheStoreMain :: Cache.EntityCacheStore a,
    cacheStoreSub :: Cache.EntityCacheStore b,
    refRelation :: RefRelationO2M
  }

----------------------------------------------------------------------------------------------------
-- OneToMany
----------------------------------------------------------------------------------------------------

-- | The `OneToMany` class defines operations for managing a one-to-many relationship between
-- two entities that can be cached.
class (Cache.EntityCache a, Cache.EntityCache b) => OneToMany a b where
  ----------------------------------------------------------------------------------------------------
  -- Default implementations
  ----------------------------------------------------------------------------------------------------

  -- | Default file path for persisting the one-to-many relationship data.
  refRelationPersist :: Proxy a -> Proxy b -> FilePath
  refRelationPersist _ _ = "./data/o2m." <> show (typeRep a) <> "." <> show (typeRep b) <> ".json"
    where
      a = Proxy @a
      b = Proxy @b

  -- | Constructs a `CacheOneToMany` with default reference data, loading from a persisted file or
  -- create new data if the file does not exists.
  construct :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheOneToMany a b)
  construct ca cb = do
    defaultO2M <- defaultRefRelationO2MData ca
    ref <- Util.decodeFileOrCreate (refRelationPersist a b) defaultO2M
    ref' <- newMVar ref
    return $ CacheOneToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  -- | Constructs a `CacheOneToMany` without loading any existing reference data, and creates
  -- a new default reference data set.
  constructWithoutRef :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheOneToMany a b)
  constructWithoutRef ca cb = do
    defaultO2M <- defaultRefRelationO2MData ca
    _ <- encodeFile (refRelationPersist a b) defaultO2M
    ref' <- newMVar defaultO2M
    return $ CacheOneToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  -- | Retrieves the one-to-many relationship map.
  getRefMap :: CacheOneToMany a b -> IO RefO2M
  getRefMap = readRefO2M . refRelation

  -- | Checks if a given main entity ID exists in the relationship.
  isIdInKey :: CacheOneToMany a b -> MainId -> IO Bool
  isIdInKey (CacheOneToMany _ _ r) mi =
    readRefO2M r >>= return . Map.member mi

  -- | Checks if a given reference entity ID exists in the cache.
  isIdInRef :: CacheOneToMany a b -> RefId -> IO Bool
  isIdInRef (CacheOneToMany _ cb _) ri = Cache.isIdInCache cb ri

  -- | Checks if a given reference entity ID exists under a specific main entity ID.
  isIdInValue :: CacheOneToMany a b -> MainId -> RefId -> IO Bool
  isIdInValue (CacheOneToMany _ _ r) mi ri = do
    readRefO2M r >>= \m ->
      case Map.lookup mi m of
        Nothing -> return False
        Just e -> return $ ri `elem` e

  -- | Retrieves all reference entity IDs associated with a main entity ID.
  getAllRefIds :: CacheOneToMany a b -> MainId -> IO [RefId]
  getAllRefIds (CacheOneToMany _ _ r) mi =
    readRefO2M r >>= \m ->
      case Map.lookup mi m of
        Nothing -> throw $ IdNotFound mi
        Just e -> return e

  -- | Retrieves all reference entity IDs associated with a main entity by its name.
  getAllRefIdsByName :: CacheOneToMany a b -> MainName -> IO [RefId]
  getAllRefIdsByName c@(CacheOneToMany ca _ _) mn =
    Cache.getIdByName ca mn >>= getAllRefIds c

  -- | Retrieves all reference entities associated with a main entity ID.
  getAllRef :: CacheOneToMany a b -> MainId -> IO [b]
  getAllRef c@(CacheOneToMany _ cb _) mi =
    getAllRefIds c mi >>= Cache.retrieveMany cb

  -- | Retrieves all reference entities associated with a main entity by its name.
  getAllRefByName :: CacheOneToMany a b -> MainName -> IO [b]
  getAllRefByName c@(CacheOneToMany ca _ _) mn =
    Cache.getIdByName ca mn >>= getAllRef c

  -- | Retrieves specific reference entities by their IDs under a main entity ID.
  getManyRef :: CacheOneToMany a b -> MainId -> [RefId] -> IO [b]
  getManyRef c@(CacheOneToMany _ cb _) mi ris =
    getAllRefIds c mi >>= Cache.retrieveMany cb . filter (`elem` ris)

  -- | Saves a new reference entity under a specific main entity ID.
  saveRef :: CacheOneToMany a b -> MainId -> Entity.EntityInput b -> IO b
  saveRef c@(CacheOneToMany _ cb r) mi inp =
    isIdInKey c mi >>= \case
      True -> do
        sb <- Cache.save cb inp
        let newRefId = Entity.getId sb
        newR <- modifyRefO2M r $ insertRefId mi newRefId
        saveRefO2M (refRelationPersist a b) newR
        return sb
      False -> throw $ IdNotFound mi
    where
      a = Proxy @a
      b = Proxy @b

  -- | Saves a new reference entity under a main entity identified by its name.
  saveRefByName :: CacheOneToMany a b -> MainName -> Entity.EntityInput b -> IO b
  saveRefByName c@(CacheOneToMany ca _ _) mn inp =
    Cache.getIdByName ca mn >>= \mi -> saveRef c mi inp

  -- | Updates an existing reference entity under a specific main entity ID.
  updateRef :: CacheOneToMany a b -> MainId -> RefId -> Entity.EntityInput b -> IO b
  updateRef c@(CacheOneToMany _ cb _) mi ri inp =
    isIdInValue c mi ri >>= \case
      True -> Cache.update cb ri inp
      False -> throw $ IdNotFound ri

  -- | Updates an existing reference entity under a main entity identified by their names.
  updateRefByName :: CacheOneToMany a b -> MainName -> RefName -> Entity.EntityInput b -> IO b
  updateRefByName c@(CacheOneToMany ca cb _) mn rn inp = do
    mi <- Cache.getIdByName ca mn
    ri <- Cache.getIdByName cb rn
    updateRef c mi ri inp

  -- | Removes a reference entity from a specific main entity ID.
  removeRef :: CacheOneToMany a b -> MainId -> RefId -> IO b
  removeRef c@(CacheOneToMany _ cb r) mi ri =
    isIdInKey c mi >>= \case
      True -> do
        rb <- Cache.remove cb ri
        newR <- modifyRefO2M r $ removeRefId mi ri
        saveRefO2M (refRelationPersist a b) newR
        return rb
      False -> throw $ IdNotFound mi
    where
      a = Proxy @a
      b = Proxy @b

  -- | Removes a reference entity from a main entity identified by their names.
  removeRefByName :: CacheOneToMany a b -> MainName -> RefName -> IO b
  removeRefByName c@(CacheOneToMany ca cb _) mn rn = do
    mi <- Cache.getIdByName ca mn
    ri <- Cache.getIdByName cb rn
    removeRef c mi ri

  -- | Binds an existing reference entity to a main entity, if not already bound.
  bindRef :: CacheOneToMany a b -> MainId -> RefId -> IO Bool
  bindRef c@(CacheOneToMany ca cb r) mi ri = do
    c1 <- Cache.isIdInCache ca mi
    c2 <- Cache.isIdInCache cb ri
    c3 <- isIdInValue c mi ri
    case (c1, c2, c3) of
      (True, True, False) -> modifyRefO2M_ r (insertRefId mi ri) >> return True
      _ -> return False

  -- | Unbinds a reference entity from a main entity, if already bound.
  unbindRef :: CacheOneToMany a b -> MainId -> RefId -> IO Bool
  unbindRef c@(CacheOneToMany ca cb r) mi ri = do
    c1 <- Cache.isIdInCache ca mi
    c2 <- Cache.isIdInCache cb ri
    c3 <- isIdInValue c mi ri
    case (c1, c2, c3) of
      (True, True, True) -> modifyRefO2M_ r (removeRefId mi ri) >> return True
      _ -> return False

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

-- | Creates a default `RefRelationO2MData` based on the cache store.
defaultRefRelationO2MData :: Cache.EntityCacheStore a -> IO RefRelationO2MData
defaultRefRelationO2MData m = do
  (_, m') <- readMVar m
  let res = Map.map (const []) m'
  return $ RefRelationO2MData res

-- | Reads the `RefO2M` map from an `MVar`.
readRefO2M :: RefRelationO2M -> IO RefO2M
readRefO2M r = readMVar r >>= return . \(RefRelationO2MData d) -> d

-- | Modifies the `RefRelationO2M` data using the provided function and returns the modified data.
modifyRefO2M :: RefRelationO2M -> (RefRelationO2MData -> RefRelationO2MData) -> IO RefRelationO2MData
modifyRefO2M r fn = modifyMVar r $ \d -> let newD = fn d in return (newD, newD)

-- | Modifies the `RefRelationO2M` data using the provided function.
modifyRefO2M_ :: RefRelationO2M -> (RefRelationO2MData -> RefRelationO2MData) -> IO ()
modifyRefO2M_ r fn = modifyMVar_ r $ \d -> let newD = fn d in return newD

-- | Saves the `RefRelationO2MData` to a file.
saveRefO2M :: FilePath -> RefRelationO2MData -> IO ()
saveRefO2M = encodeFile

-- | Inserts a reference ID into the map under the specified main ID.
insertRefId :: MainId -> RefId -> RefRelationO2MData -> RefRelationO2MData
insertRefId mi ri (RefRelationO2MData m) =
  RefRelationO2MData um
  where
    um = Map.insertWith (++) mi [ri] m

-- | Removes a reference ID from the map under the specified main ID.
removeRefId :: MainId -> RefId -> RefRelationO2MData -> RefRelationO2MData
removeRefId mi ri (RefRelationO2MData m) =
  RefRelationO2MData um
  where
    um = Map.update fn mi m
    fn :: [RefId] -> Maybe [RefId]
    fn ris =
      let newRis = filter (/= ri) ris
       in if null newRis then Nothing else Just newRis
