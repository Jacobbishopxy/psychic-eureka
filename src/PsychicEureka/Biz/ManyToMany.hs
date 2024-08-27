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

type LeftId = Id

type RightId = Id

type LeftName = String

type RightName = String

type RefM2ML = Map.Map LeftId [RightId]

type RefM2MR = Map.Map RightId [LeftId]

data RefRelationM2MData = RefRelationM2MData
  { l2r :: RefM2ML,
    r2l :: RefM2MR
  }
  deriving (Generic, ToJSON, FromJSON)

type RefRelationM2M = MVar RefRelationM2MData

data CacheManyToMany a b = CacheManyToMany
  { cacheStoreLeft :: Cache.EntityCacheStore a,
    cacheStoreRight :: Cache.EntityCacheStore b,
    refRelation :: RefRelationM2M
  }

----------------------------------------------------------------------------------------------------
-- ManyToMany
----------------------------------------------------------------------------------------------------

class (Cache.EntityCache a, Cache.EntityCache b) => ManyToMany a b where
  ----------------------------------------------------------------------------------------------------
  -- default impl
  ----------------------------------------------------------------------------------------------------

  -- default persistence directory
  refRelationPersist :: Proxy a -> Proxy b -> FilePath
  refRelationPersist _ _ = "./data/m2m." <> show (typeRep a) <> "." <> show (typeRep b) <> ".json"
    where
      a = Proxy @a
      b = Proxy @b

  construct :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheManyToMany a b)
  construct ca cb = do
    defaultM2M <- defaultRefRelationM2MData ca cb
    ref <- Util.decodeFileOrCreate (refRelationPersist a b) defaultM2M
    ref' <- newMVar ref
    return $ CacheManyToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  constructWithoutRef :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheManyToMany a b)
  constructWithoutRef ca cb = do
    defaultM2M <- defaultRefRelationM2MData ca cb
    _ <- encodeFile (refRelationPersist a b) defaultM2M
    ref' <- newMVar defaultM2M
    return $ CacheManyToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  getRefMap :: CacheManyToMany a b -> IO RefRelationM2MData
  getRefMap = readRefM2M . refRelation

  getRefMapL :: CacheManyToMany a b -> IO RefM2ML
  getRefMapL (CacheManyToMany _ _ r) = readRefM2M r >>= return . l2r

  getRefMapR :: CacheManyToMany a b -> IO RefM2MR
  getRefMapR (CacheManyToMany _ _ r) = readRefM2M r >>= return . r2l

  isIdInKeyL :: CacheManyToMany a b -> LeftId -> IO Bool
  isIdInKeyL (CacheManyToMany _ _ r) li =
    readRefM2ML r >>= return . Map.member li

  isIdInKeyR :: CacheManyToMany a b -> RightId -> IO Bool
  isIdInKeyR (CacheManyToMany _ _ r) ri =
    readRefM2MR r >>= return . Map.member ri

  isIdInValueL :: CacheManyToMany a b -> LeftId -> RightId -> IO Bool
  isIdInValueL (CacheManyToMany _ _ r) li ri =
    readRefM2ML r >>= \m ->
      case Map.lookup li m of
        Nothing -> return False
        Just e -> return $ ri `elem` e

  isIdInValueR :: CacheManyToMany a b -> RightId -> LeftId -> IO Bool
  isIdInValueR (CacheManyToMany _ _ r) ri li =
    readRefM2MR r >>= \m ->
      case Map.lookup ri m of
        Nothing -> return False
        Just e -> return $ li `elem` e

  getAllRefIdsL :: CacheManyToMany a b -> LeftId -> IO [RightId]
  getAllRefIdsL (CacheManyToMany _ _ r) li =
    readRefM2ML r >>= \m ->
      case Map.lookup li m of
        Nothing -> throw $ IdNotFound li
        Just e -> return e

  getAllRefIdsR :: CacheManyToMany a b -> RightId -> IO [LeftId]
  getAllRefIdsR (CacheManyToMany _ _ r) ri =
    readRefM2MR r >>= \m ->
      case Map.lookup ri m of
        Nothing -> throw $ IdNotFound ri
        Just e -> return e

  getAllRefIdsByNameL :: CacheManyToMany a b -> LeftName -> IO [RightId]
  getAllRefIdsByNameL c@(CacheManyToMany ca _ _) ln =
    Cache.getIdByName ca ln >>= getAllRefIdsL c

  getAllRefIdsByNameR :: CacheManyToMany a b -> RightName -> IO [LeftId]
  getAllRefIdsByNameR c@(CacheManyToMany _ cb _) rn =
    Cache.getIdByName cb rn >>= getAllRefIdsR c

  getAllRefL :: CacheManyToMany a b -> LeftId -> IO [b]
  getAllRefL c@(CacheManyToMany _ cb _) li =
    getAllRefIdsL c li >>= Cache.retrieveMany cb

  getAllRefR :: CacheManyToMany a b -> RightId -> IO [a]
  getAllRefR c@(CacheManyToMany ca _ _) ri =
    getAllRefIdsR c ri >>= Cache.retrieveMany ca

  getAllRefByNameL :: CacheManyToMany a b -> LeftName -> IO [b]
  getAllRefByNameL c@(CacheManyToMany ca _ _) ln =
    Cache.getIdByName ca ln >>= getAllRefL c

  getAllRefByNameR :: CacheManyToMany a b -> RightName -> IO [a]
  getAllRefByNameR c@(CacheManyToMany _ cb _) rn =
    Cache.getIdByName cb rn >>= getAllRefR c

  getManyRefL :: CacheManyToMany a b -> LeftId -> [RightId] -> IO [b]
  getManyRefL c@(CacheManyToMany _ cb _) li ris =
    getAllRefIdsL c li >>= Cache.retrieveMany cb . filter (`elem` ris)

  getManyRefR :: CacheManyToMany a b -> RightId -> [LeftId] -> IO [a]
  getManyRefR c@(CacheManyToMany ca _ _) ri lis =
    getAllRefIdsR c ri >>= Cache.retrieveMany ca . filter (`elem` lis)

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

  saveRefByNameL :: CacheManyToMany a b -> LeftName -> Entity.EntityInput b -> IO b
  saveRefByNameL c@(CacheManyToMany ca _ _) ln inp =
    Cache.getIdByName ca ln >>= \li -> saveRefL c li inp

  saveRefByNameR :: CacheManyToMany a b -> RightName -> Entity.EntityInput a -> IO a
  saveRefByNameR c@(CacheManyToMany _ cb _) rn inp =
    Cache.getIdByName cb rn >>= \ri -> saveRefR c ri inp

  updateRefL :: CacheManyToMany a b -> LeftId -> RightId -> Entity.EntityInput b -> IO b
  updateRefL c@(CacheManyToMany _ cb _) li ri inp =
    isIdInValueL c li ri >>= \case
      True -> Cache.update cb ri inp
      False -> throw $ IdNotFound ri

  updateRefR :: CacheManyToMany a b -> RightId -> LeftId -> Entity.EntityInput a -> IO a
  updateRefR c@(CacheManyToMany ca _ _) ri li inp =
    isIdInValueR c ri li >>= \case
      True -> Cache.update ca li inp
      False -> throw $ IdNotFound li

  updateRefByNameL :: CacheManyToMany a b -> LeftName -> RightName -> Entity.EntityInput b -> IO b
  updateRefByNameL c@(CacheManyToMany ca cb _) ln rn inp = do
    li <- Cache.getIdByName ca ln
    ri <- Cache.getIdByName cb rn
    updateRefL c li ri inp

  updateRefByNameR :: CacheManyToMany a b -> RightName -> LeftName -> Entity.EntityInput a -> IO a
  updateRefByNameR c@(CacheManyToMany ca cb _) rn ln inp = do
    ri <- Cache.getIdByName cb rn
    li <- Cache.getIdByName ca ln
    updateRefR c ri li inp

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

  bindRefL :: CacheManyToMany a b -> LeftId -> RightId -> IO Bool
  bindRefL c@(CacheManyToMany ca cb r) li ri = do
    c1 <- Cache.isIdInCache ca li
    c2 <- Cache.isIdInCache cb ri
    c3 <- isIdInValueL c li ri
    case (c1, c2, c3) of
      (True, True, False) -> modifyRefM2M_ r (insertRefIdL li ri) >> return True
      _ -> return False

  bindRefR :: CacheManyToMany a b -> RightId -> LeftId -> IO Bool
  bindRefR c@(CacheManyToMany ca cb r) ri li = do
    c1 <- Cache.isIdInCache cb ri
    c2 <- Cache.isIdInCache ca li
    c3 <- isIdInValueR c ri li
    case (c1, c2, c3) of
      (True, True, False) -> modifyRefM2M_ r (insertRefIdR ri li) >> return True
      _ -> return False

  unbindRefL :: CacheManyToMany a b -> LeftId -> RightId -> IO Bool
  unbindRefL c@(CacheManyToMany ca cb r) li ri = do
    c1 <- Cache.isIdInCache ca li
    c2 <- Cache.isIdInCache cb ri
    c3 <- isIdInValueL c li ri
    case (c1, c2, c3) of
      (True, True, True) -> modifyRefM2M_ r (removeRefIdL li ri) >> return True
      _ -> return False

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

defaultRefRelationM2MData :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO RefRelationM2MData
defaultRefRelationM2MData ml mr = do
  (_, ml') <- readMVar ml
  (_, mr') <- readMVar mr
  let l2r' = Map.map (const []) ml'
      r2l' = Map.map (const []) mr'
  return $ RefRelationM2MData l2r' r2l'

readRefM2M :: RefRelationM2M -> IO RefRelationM2MData
readRefM2M = readMVar

readRefM2ML :: RefRelationM2M -> IO RefM2ML
readRefM2ML r = readMVar r >>= return . l2r

readRefM2MR :: RefRelationM2M -> IO RefM2MR
readRefM2MR r = readMVar r >>= return . r2l

modifyRefM2M :: RefRelationM2M -> (RefRelationM2MData -> RefRelationM2MData) -> IO RefRelationM2MData
modifyRefM2M r fn = modifyMVar r $ \d -> let newD = fn d in return (newD, newD)

modifyRefM2M_ :: RefRelationM2M -> (RefRelationM2MData -> RefRelationM2MData) -> IO ()
modifyRefM2M_ r fn = modifyMVar_ r $ \d -> let newD = fn d in return newD

insertRefIdL :: LeftId -> RightId -> RefRelationM2MData -> RefRelationM2MData
insertRefIdL li ri d =
  let ld = l2r d
      newLd = Map.insertWith (++) li [ri] ld
   in RefRelationM2MData newLd (r2l d)

insertRefIdR :: RightId -> LeftId -> RefRelationM2MData -> RefRelationM2MData
insertRefIdR ri li d =
  let rd = r2l d
      newRd = Map.insertWith (++) ri [li] rd
   in RefRelationM2MData (l2r d) newRd

saveRefM2M :: FilePath -> RefRelationM2MData -> IO ()
saveRefM2M = encodeFile

removeRefIdL :: LeftId -> RightId -> RefRelationM2MData -> RefRelationM2MData
removeRefIdL li ri d =
  let ld = l2r d
      newLd = Map.update (removeIdFromValue ri) li ld
   in RefRelationM2MData newLd (r2l d)

removeRefIdR :: RightId -> LeftId -> RefRelationM2MData -> RefRelationM2MData
removeRefIdR ri li d =
  let rd = r2l d
      newRd = Map.update (removeIdFromValue li) ri rd
   in RefRelationM2MData (r2l d) newRd

removeIdFromValue :: Id -> [Id] -> Maybe [Id]
removeIdFromValue i ris =
  let newIds = filter (/= i) ris
   in if null newIds then Nothing else Just newIds
