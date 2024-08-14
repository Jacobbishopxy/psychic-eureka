{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

-- file: OneToMany.hs
-- author: Jacob Xie
-- date: 2024/08/12 13:42:45 Monday
-- brief:

module PsychicEureka.Biz.OneToMany where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (throw)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import PsychicEureka.Biz.Common (RefEntity, getRef)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import PsychicEureka.Error (EurekaError (..))
import PsychicEureka.Util (Id)

type MainId = Id

type RefId = Id

type MainName = String

type RefName = String

type RefRelation = MVar (Map.Map MainId [RefId])

data CacheOneToMany a b = CacheOneToMany
  { cacheStoreMain :: Cache.EntityCacheStore a,
    cacheStoreSub :: Cache.EntityCacheStore b,
    refRelation :: RefRelation
  }

class (Cache.EntityCache a, RefEntity b, Cache.EntityCache b) => OneToMany a b where
  construct :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheOneToMany a b)
  construct ca cb = do
    (_, ma) <- readMVar ca
    (_, mb) <- readMVar cb

    ref <- newMVar $ Map.mapWithKey (\k _ -> findRefs k mb) ma
    return $ CacheOneToMany ca cb ref
    where
      matchRef _ka (_kb, _vb) = getRef _vb >>= \rf -> if rf == _ka then Just _kb else Nothing
      findRefs _ka _mb = mapMaybe (matchRef _ka) (Map.assocs _mb)

  getRefMap :: CacheOneToMany a b -> IO (Map.Map MainId [RefId])
  getRefMap = readMVar . refRelation

  isIdInKey :: CacheOneToMany a b -> MainId -> IO Bool
  isIdInKey (CacheOneToMany _ _ r) mi =
    readMVar r >>= return . Map.member mi

  isIdInValue :: CacheOneToMany a b -> MainId -> RefId -> IO Bool
  isIdInValue (CacheOneToMany _ _ r) mi ri = do
    readMVar r >>= \m ->
      case Map.lookup mi m of
        Nothing -> return False
        Just e -> return $ ri `elem` e

  getAllRefIds :: CacheOneToMany a b -> MainId -> IO [RefId]
  getAllRefIds (CacheOneToMany _ _ r) mi =
    readMVar r >>= \m ->
      case Map.lookup mi m of
        Nothing -> throw $ IdNotFound mi
        Just e -> return e

  getAllRefIdsByName :: CacheOneToMany a b -> MainName -> IO [RefId]
  getAllRefIdsByName c@(CacheOneToMany ca _ _) mn =
    Cache.getIdByName ca mn >>= getAllRefIds c

  getAllRef :: CacheOneToMany a b -> MainId -> IO [b]
  getAllRef c@(CacheOneToMany _ cb _) mi =
    getAllRefIds c mi >>= Cache.retrieveMany cb

  getAllRefByName :: CacheOneToMany a b -> MainName -> IO [b]
  getAllRefByName c@(CacheOneToMany ca _ _) mn =
    Cache.getIdByName ca mn >>= getAllRef c

  getManyRef :: CacheOneToMany a b -> MainId -> [RefId] -> IO [b]
  getManyRef c@(CacheOneToMany _ cb _) mi ris =
    getAllRefIds c mi >>= Cache.retrieveMany cb . filter (`notElem` ris)

  saveRef :: CacheOneToMany a b -> MainId -> Entity.EntityInput b -> IO b
  saveRef c@(CacheOneToMany _ cb r) mi inp =
    isIdInKey c mi >>= \case
      True -> do
        b <- Cache.save cb inp
        let newRefId = Entity.getId b
        modifyMVar_ r $ \m ->
          let updatedVal = newRefId : (m Map.! mi)
           in return $ Map.insert mi updatedVal m
        return b
      False -> throw $ IdNotFound mi

  saveRefByName :: CacheOneToMany a b -> MainName -> Entity.EntityInput b -> IO b
  saveRefByName c@(CacheOneToMany ca _ _) mn inp =
    Cache.getIdByName ca mn >>= \mi -> saveRef c mi inp

  updateRef :: CacheOneToMany a b -> MainId -> RefId -> Entity.EntityInput b -> IO b
  updateRef c@(CacheOneToMany _ cb _) mi ri inp =
    isIdInValue c mi ri >>= \case
      True -> Cache.update cb ri inp
      False -> throw $ IdNotFound ri

  updateRefByName :: CacheOneToMany a b -> MainName -> RefName -> Entity.EntityInput b -> IO b
  updateRefByName c@(CacheOneToMany ca cb _) mn rn inp = do
    mi <- Cache.getIdByName ca mn
    ri <- Cache.getIdByName cb rn
    updateRef c mi ri inp

  removeRef :: CacheOneToMany a b -> MainId -> RefId -> IO b
  removeRef c@(CacheOneToMany _ cb r) mi ri =
    isIdInKey c mi >>= \case
      True -> do
        b <- Cache.remove cb ri
        modifyMVar_ r $ \m ->
          let updatedVal = filter (/= ri) (m Map.! mi)
           in return $ Map.insert mi updatedVal m
        return b
      False -> throw $ IdNotFound ri

  removeRefByName :: CacheOneToMany a b -> MainName -> RefName -> IO b
  removeRefByName c@(CacheOneToMany ca cb _) mn rn = do
    mi <- Cache.getIdByName ca mn
    ri <- Cache.getIdByName cb rn
    removeRef c mi ri
