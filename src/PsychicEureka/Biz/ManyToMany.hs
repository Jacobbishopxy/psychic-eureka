{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- file: ManyToMany.hs
-- author: Jacob Xie
-- date: 2024/08/26 08:27:41 Monday
-- brief:

module PsychicEureka.Biz.ManyToMany
  ( CacheManyToMany (..),
    ManyToMany (..),
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
import PsychicEureka.Error (EurekaError (IdNotFound))
import qualified PsychicEureka.Internal.Util as Util
import PsychicEureka.Util (Id)

----------------------------------------------------------------------------------------------------
-- Types & Newtype
----------------------------------------------------------------------------------------------------

-- | Type alias for Left-side entity ID in the many-to-many relationship.
type LeftId = Id

-- | Type alias for Right-side entity ID in the many-to-many relationship.
type RightId = Id

-- | Type alias for Left-side entity name.
type LeftName = String

-- | Type alias for Right-side entity name.
type RightName = String

-- | A map from LeftId to a list of RightIds.
type RefM2ML = Map.Map LeftId [RightId]

-- | A map from RightId to a list of LeftIds.
type RefM2MR = Map.Map RightId [LeftId]

-- | Data structure representing the many-to-many relationship data.
data RefRelationM2MData = RefRelationM2MData
  { l2r :: RefM2ML,
    r2l :: RefM2MR
  }
  deriving (Generic, ToJSON, FromJSON)

-- | An MVar-wrapped data structure for thread-safe operations on many-to-many relationships.
type RefRelationM2M = MVar RefRelationM2MData

-- | The main data structure for caching and managing many-to-many relationships.
data CacheManyToMany a b = CacheManyToMany
  { cacheStoreLeft :: Cache.EntityCacheStore a,
    cacheStoreRight :: Cache.EntityCacheStore b,
    refRelation :: RefRelationM2M
  }

----------------------------------------------------------------------------------------------------
-- ManyToMany
----------------------------------------------------------------------------------------------------

-- | Class for managing many-to-many relationships between two entities.
class (Cache.EntityCache a, Cache.EntityCache b) => ManyToMany a b where
  ----------------------------------------------------------------------------------------------------
  -- default impl
  ----------------------------------------------------------------------------------------------------

  -- | Default file path for persisting the relationship data.
  refRelationPersist :: Proxy a -> Proxy b -> FilePath
  refRelationPersist _ _ = "./data/m2m." <> show (typeRep a) <> "." <> show (typeRep b) <> ".json"
    where
      a = Proxy @a
      b = Proxy @b

  -- | Construct a new CacheManyToMany with loaded relationship data from file.
  construct :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheManyToMany a b)
  construct ca cb = do
    defaultM2M <- defaultRefRelationM2MData ca cb
    ref <- Util.decodeFileOrCreate (refRelationPersist a b) defaultM2M
    ref' <- newMVar ref
    return $ CacheManyToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  -- | Construct a new CacheManyToMany without loading relationship data from file.
  constructWithoutRef :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheManyToMany a b)
  constructWithoutRef ca cb = do
    defaultM2M <- defaultRefRelationM2MData ca cb
    _ <- encodeFile (refRelationPersist a b) defaultM2M
    ref' <- newMVar defaultM2M
    return $ CacheManyToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  -- | Retrieve the entire relationship data.
  getRefMap :: CacheManyToMany a b -> IO RefRelationM2MData
  getRefMap = readRefM2M . refRelation

  -- | Retrieve the map of LeftId to RightIds
  getRefMapL :: CacheManyToMany a b -> IO RefM2ML
  getRefMapL (CacheManyToMany _ _ r) = readRefM2M r >>= return . l2r

  -- | Retrieve the map of RightId to LeftIds
  getRefMapR :: CacheManyToMany a b -> IO RefM2MR
  getRefMapR (CacheManyToMany _ _ r) = readRefM2M r >>= return . r2l

  -- | Check if a LeftId exists in the relationship map.
  isIdInKeyL :: CacheManyToMany a b -> LeftId -> IO Bool
  isIdInKeyL (CacheManyToMany _ _ r) li =
    readRefM2ML r >>= return . Map.member li

  -- | Check if a RightId exists in the relationship map.
  isIdInKeyR :: CacheManyToMany a b -> RightId -> IO Bool
  isIdInKeyR (CacheManyToMany _ _ r) ri =
    readRefM2MR r >>= return . Map.member ri

  -- | Check if a specific RightId is associated with a given LeftId.
  isIdInValueL :: CacheManyToMany a b -> LeftId -> RightId -> IO Bool
  isIdInValueL (CacheManyToMany _ _ r) li ri =
    readRefM2ML r >>= \m ->
      case Map.lookup li m of
        Nothing -> return False
        Just e -> return $ ri `elem` e

  -- | Check if a specific LeftId is associated with a given RightId.
  isIdInValueR :: CacheManyToMany a b -> RightId -> LeftId -> IO Bool
  isIdInValueR (CacheManyToMany _ _ r) ri li =
    readRefM2MR r >>= \m ->
      case Map.lookup ri m of
        Nothing -> return False
        Just e -> return $ li `elem` e

  -- | Retrieve all RightIds associated with a given LeftId.
  getAllRefIdsL :: CacheManyToMany a b -> LeftId -> IO [RightId]
  getAllRefIdsL (CacheManyToMany _ _ r) li =
    readRefM2ML r >>= \m ->
      case Map.lookup li m of
        Nothing -> throw $ IdNotFound li
        Just e -> return e

  -- | Retrieve all LeftIds associated with a given RightId.
  getAllRefIdsR :: CacheManyToMany a b -> RightId -> IO [LeftId]
  getAllRefIdsR (CacheManyToMany _ _ r) ri =
    readRefM2MR r >>= \m ->
      case Map.lookup ri m of
        Nothing -> throw $ IdNotFound ri
        Just e -> return e

  -- | Retrieve all RightIds associated with a given LeftName.
  getAllRefIdsByNameL :: CacheManyToMany a b -> LeftName -> IO [RightId]
  getAllRefIdsByNameL c@(CacheManyToMany ca _ _) ln =
    Cache.getIdByName ca ln >>= getAllRefIdsL c

  -- | Retrieve all LeftIds associated with a given RightName.
  getAllRefIdsByNameR :: CacheManyToMany a b -> RightName -> IO [LeftId]
  getAllRefIdsByNameR c@(CacheManyToMany _ cb _) rn =
    Cache.getIdByName cb rn >>= getAllRefIdsR c

  -- | Retrieve all Right entities associated with a given LeftId.
  getAllRefL :: CacheManyToMany a b -> LeftId -> IO [b]
  getAllRefL c@(CacheManyToMany _ cb _) li =
    getAllRefIdsL c li >>= Cache.retrieveMany cb

  -- | Retrieve all Left entities associated with a given RightId.
  getAllRefR :: CacheManyToMany a b -> RightId -> IO [a]
  getAllRefR c@(CacheManyToMany ca _ _) ri =
    getAllRefIdsR c ri >>= Cache.retrieveMany ca

  -- | Retrieve all Right entities associated with a given LeftName.
  getAllRefByNameL :: CacheManyToMany a b -> LeftName -> IO [b]
  getAllRefByNameL c@(CacheManyToMany ca _ _) ln =
    Cache.getIdByName ca ln >>= getAllRefL c

  -- | Retrieve all Left entities associated with a given RightName.
  getAllRefByNameR :: CacheManyToMany a b -> RightName -> IO [a]
  getAllRefByNameR c@(CacheManyToMany _ cb _) rn =
    Cache.getIdByName cb rn >>= getAllRefR c

  -- | Retrieves multiple entities of type `b` associated with a given LeftId.
  getManyRefL :: CacheManyToMany a b -> LeftId -> [RightId] -> IO [b]
  getManyRefL c@(CacheManyToMany _ cb _) li ris =
    getAllRefIdsL c li >>= Cache.retrieveMany cb . filter (`elem` ris)

  -- | Retrieves multiple entities of type `a` associated with a given RightId.
  getManyRefR :: CacheManyToMany a b -> RightId -> [LeftId] -> IO [a]
  getManyRefR c@(CacheManyToMany ca _ _) ri lis =
    getAllRefIdsR c ri >>= Cache.retrieveMany ca . filter (`elem` lis)

  -- | Saves a new entity of type `b` and associates it with a given `LeftId`.
  saveRefL :: CacheManyToMany a b -> LeftId -> Entity.EntityInput b -> IO b
  saveRefL c@(CacheManyToMany _ cb r) li inp =
    isIdInKeyL c li >>= \case
      True -> do
        sb <- Cache.save cb inp
        let newRefId = Entity.getId sb
        res <- modifyRefM2M r $ insertRefIdL li newRefId
        saveRefM2M (refRelationPersist a b) res
        return sb
      False -> throw $ IdNotFound li
    where
      a = Proxy @a
      b = Proxy @b

  -- | Saves a new entity of type `a` and associates it with a given `RightId`.
  saveRefR :: CacheManyToMany a b -> RightId -> Entity.EntityInput a -> IO a
  saveRefR c@(CacheManyToMany ca _ r) ri inp =
    isIdInKeyR c ri >>= \case
      True -> do
        sa <- Cache.save ca inp
        let newRefId = Entity.getId sa
        res <- modifyRefM2M r $ insertRefIdR ri newRefId
        saveRefM2M (refRelationPersist a b) res
        return sa
      False -> throw $ IdNotFound ri
    where
      a = Proxy @a
      b = Proxy @b

  -- | Saves a new entity of type `b` by left entity's name and associates it with the found `LeftId`.
  saveRefByNameL :: CacheManyToMany a b -> LeftName -> Entity.EntityInput b -> IO b
  saveRefByNameL c@(CacheManyToMany ca _ _) ln inp =
    Cache.getIdByName ca ln >>= \li -> saveRefL c li inp

  -- | Saves a new entity of type `a` by right entity's name and associates it with the found `RightId`.
  saveRefByNameR :: CacheManyToMany a b -> RightName -> Entity.EntityInput a -> IO a
  saveRefByNameR c@(CacheManyToMany _ cb _) rn inp =
    Cache.getIdByName cb rn >>= \ri -> saveRefR c ri inp

  -- | Updates an existing entity of type `b` associated with a given `LeftId` and `RightId`.
  updateRefL :: CacheManyToMany a b -> LeftId -> RightId -> Entity.EntityInput b -> IO b
  updateRefL c@(CacheManyToMany _ cb _) li ri inp =
    isIdInValueL c li ri >>= \case
      True -> Cache.update cb ri inp
      False -> throw $ IdNotFound ri

  -- | Updates an existing entity of type `a` associated with a given `RightId` and `LeftId`.
  updateRefR :: CacheManyToMany a b -> RightId -> LeftId -> Entity.EntityInput a -> IO a
  updateRefR c@(CacheManyToMany ca _ _) ri li inp =
    isIdInValueR c ri li >>= \case
      True -> Cache.update ca li inp
      False -> throw $ IdNotFound li

  -- | Updates an existing entity of type `b` by left and right entity names.
  updateRefByNameL :: CacheManyToMany a b -> LeftName -> RightName -> Entity.EntityInput b -> IO b
  updateRefByNameL c@(CacheManyToMany ca cb _) ln rn inp = do
    li <- Cache.getIdByName ca ln
    ri <- Cache.getIdByName cb rn
    updateRefL c li ri inp

  -- | Updates an existing entity of type `a` by right and left entity names.
  updateRefByNameR :: CacheManyToMany a b -> RightName -> LeftName -> Entity.EntityInput a -> IO a
  updateRefByNameR c@(CacheManyToMany ca cb _) rn ln inp = do
    ri <- Cache.getIdByName cb rn
    li <- Cache.getIdByName ca ln
    updateRefR c ri li inp

  -- | Removes the association between a `LeftId` and `RightId`, and deletes the `RightId` entity.
  removeRefL :: CacheManyToMany a b -> LeftId -> RightId -> IO b
  removeRefL c@(CacheManyToMany _ cb r) li ri =
    isIdInKeyL c li >>= \case
      True -> do
        rb <- Cache.remove cb ri
        res <- modifyRefM2M r $ removeRefIdL li ri
        saveRefM2M (refRelationPersist a b) res
        return rb
      False -> throw $ IdNotFound li
    where
      a = Proxy @a
      b = Proxy @b

  -- | Removes the association between a `RightId` and `LeftId`, and deletes the `LeftId` entity.
  removeRefR :: CacheManyToMany a b -> RightId -> LeftId -> IO a
  removeRefR c@(CacheManyToMany ca _ r) ri li =
    isIdInKeyR c ri >>= \case
      True -> do
        ra <- Cache.remove ca li
        res <- modifyRefM2M r $ removeRefIdR ri li
        saveRefM2M (refRelationPersist a b) res
        return ra
      False -> throw $ IdNotFound ri
    where
      a = Proxy @a
      b = Proxy @b

  -- | Binds an existing LeftId entity to a RightId entity, if not already bound.
  bindRefL :: CacheManyToMany a b -> LeftId -> RightId -> IO Bool
  bindRefL c@(CacheManyToMany ca cb r) li ri = do
    c1 <- Cache.isIdInCache ca li
    c2 <- Cache.isIdInCache cb ri
    c3 <- isIdInValueL c li ri
    case (c1, c2, c3) of
      (True, True, False) -> modifyRefM2M_ r (insertRefIdL li ri) >> return True
      _ -> return False

  -- | Binds an existing RightId entity to a LeftId entity, if not already bound.
  bindRefR :: CacheManyToMany a b -> RightId -> LeftId -> IO Bool
  bindRefR c@(CacheManyToMany ca cb r) ri li = do
    c1 <- Cache.isIdInCache cb ri
    c2 <- Cache.isIdInCache ca li
    c3 <- isIdInValueR c ri li
    case (c1, c2, c3) of
      (True, True, False) -> modifyRefM2M_ r (insertRefIdR ri li) >> return True
      _ -> return False

  -- | Unbinds an existing LeftId entity from a RightId entity, if not already bound.
  unbindRefL :: CacheManyToMany a b -> LeftId -> RightId -> IO Bool
  unbindRefL c@(CacheManyToMany ca cb r) li ri = do
    c1 <- Cache.isIdInCache ca li
    c2 <- Cache.isIdInCache cb ri
    c3 <- isIdInValueL c li ri
    case (c1, c2, c3) of
      (True, True, True) -> modifyRefM2M_ r (removeRefIdL li ri) >> return True
      _ -> return False

  -- | Unbinds an existing RightId entity from a LeftId entity, if not already bound.
  unbindRefR :: CacheManyToMany a b -> RightId -> LeftId -> IO Bool
  unbindRefR c@(CacheManyToMany ca cb r) ri li = do
    c1 <- Cache.isIdInCache ca li
    c2 <- Cache.isIdInCache cb ri
    c3 <- isIdInValueL c li ri
    case (c1, c2, c3) of
      (True, True, True) -> modifyRefM2M_ r (removeRefIdR ri li) >> return True
      _ -> return False

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

-- | Provides the default many-to-many relationship map by initializing
--   both sides (`l2r` and `r2l`) with empty lists for each ID in the given
--   entity caches.
defaultRefRelationM2MData :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO RefRelationM2MData
defaultRefRelationM2MData ml mr = do
  (_, ml') <- readMVar ml
  (_, mr') <- readMVar mr
  let l2r' = Map.map (const []) ml'
      r2l' = Map.map (const []) mr'
  return $ RefRelationM2MData l2r' r2l'

-- | Reads the entire many-to-many relationship data from a thread-safe reference.
readRefM2M :: RefRelationM2M -> IO RefRelationM2MData
readRefM2M = readMVar

-- | Reads the left-to-right mapping (`l2r`) from the many-to-many relationship data.
readRefM2ML :: RefRelationM2M -> IO RefM2ML
readRefM2ML r = readMVar r >>= return . l2r

-- | Reads the right-to-left mapping (`r2l`) from the many-to-many relationship data.
readRefM2MR :: RefRelationM2M -> IO RefM2MR
readRefM2MR r = readMVar r >>= return . r2l

-- | Modifies the many-to-many relationship data in a thread-safe manner.
--   The provided function `fn` is applied to the current data, and the updated
--   data is returned.
modifyRefM2M :: RefRelationM2M -> (RefRelationM2MData -> RefRelationM2MData) -> IO RefRelationM2MData
modifyRefM2M r fn = modifyMVar r $ \d -> let newD = fn d in return (newD, newD)

-- | Modifies the many-to-many relationship data in a thread-safe manner without returning the result.
modifyRefM2M_ :: RefRelationM2M -> (RefRelationM2MData -> RefRelationM2MData) -> IO ()
modifyRefM2M_ r fn = modifyMVar_ r $ \d -> let newD = fn d in return newD

-- | Inserts a `RightId` into the list of associated IDs for a given `LeftId`
--   and updates the `RightId` with the associated `LeftId`.
insertRefIdL :: LeftId -> RightId -> RefRelationM2MData -> RefRelationM2MData
insertRefIdL li ri (RefRelationM2MData ld rd) =
  let newLd = Map.insertWith (++) li [ri] ld
      newRd = Map.insertWith (++) ri [li] rd
   in RefRelationM2MData newLd newRd

-- | Inserts a `LeftId` into the list of associated IDs for a given `RightId`
--   and updates the `LeftId` with the associated `RightId`.
insertRefIdR :: RightId -> LeftId -> RefRelationM2MData -> RefRelationM2MData
insertRefIdR ri li (RefRelationM2MData ld rd) =
  let newLd = Map.insertWith (++) li [ri] ld
      newRd = Map.insertWith (++) ri [li] rd
   in RefRelationM2MData newLd newRd

-- | Saves the many-to-many relationship data to a specified file.
saveRefM2M :: FilePath -> RefRelationM2MData -> IO ()
saveRefM2M = encodeFile

-- | Removes the association between a `LeftId` and a `RightId` from the
--   left-to-right map and updates the right-to-left map accordingly.
removeRefIdL :: LeftId -> RightId -> RefRelationM2MData -> RefRelationM2MData
removeRefIdL li ri (RefRelationM2MData ld rd) =
  let newLd = Map.update (removeIdFromValue ri) li ld
      newRd = Map.update (removeIdFromValue li) ri rd
   in RefRelationM2MData newLd newRd

-- | Removes the association between a `RightId` and a `LeftId` from the
--   right-to-left map and updates the left-to-right map accordingly.
removeRefIdR :: RightId -> LeftId -> RefRelationM2MData -> RefRelationM2MData
removeRefIdR ri li (RefRelationM2MData ld rd) =
  let newLd = Map.update (removeIdFromValue ri) li ld
      newRd = Map.update (removeIdFromValue li) ri rd
   in RefRelationM2MData newLd newRd

-- | Helper function to remove a specific ID from a list of IDs.
--   If the resulting list is empty, `Nothing` is returned, effectively removing
--   the key from the map.
removeIdFromValue :: Id -> [Id] -> Maybe [Id]
removeIdFromValue i ris =
  let newIds = filter (/= i) ris
   in if null newIds then Nothing else Just newIds
