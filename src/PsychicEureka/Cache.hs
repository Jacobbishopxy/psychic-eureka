{-# LANGUAGE LambdaCase #-}

-- file: Cache.hs
-- author: Jacob Xie
-- date: 2024/07/27 15:58:21 Saturday
-- brief:

module PsychicEureka.Cache where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (throw)
import qualified Data.Map as Map
import qualified PsychicEureka.Entity as Entity
import PsychicEureka.Error (EurekaError (..))
import PsychicEureka.Util (Id)

----------------------------------------------------------------------------------------------------
-- EntityCache
----------------------------------------------------------------------------------------------------

type NameIdMapping = Map.Map String Id

type IdEntityMapping = Map.Map Id

type EntityCacheStore a = MVar (NameIdMapping, IdEntityMapping a)

class (Entity.Entity a) => EntityCache a where
  -- init
  initialize :: IO (EntityCacheStore a)
  initialize =
    Entity.retrieveAll >>= \a ->
      let m1 = Map.fromList [(Entity.getName e, Entity.getId e) | e <- a]
          m2 = Map.fromList [(Entity.getId e, e) | e <- a]
       in newMVar (m1, m2)

  -- get name id mapping
  getNameMap :: EntityCacheStore a -> IO (Map.Map String Id)
  getNameMap mvar = readMVar mvar >>= return . fst

  -- get id by name
  getIdByName :: EntityCacheStore a -> String -> IO Id
  getIdByName mvar n =
    readMVar mvar >>= \(m, _) ->
      case Map.lookup n m of
        Nothing -> throw $ EntityNotFound n
        Just i -> return i

  -- get id by name, return `Maybe`
  -- (this function is used by the following functions;
  -- instead of throwing error, a `Maybe` type is more flexible
  getIdByNameM :: EntityCacheStore a -> String -> IO (Maybe Id)
  getIdByNameM mvar n = readMVar mvar >>= return . Map.lookup n . fst

  -- retrieve an `a` from a caching
  retrieve :: EntityCacheStore a -> Id -> IO a
  retrieve mvar i =
    readMVar mvar >>= \(_, m) ->
      case Map.lookup i m of
        Nothing -> throw $ EntityNotFound $ show i
        Just e -> return e

  -- retrieve all `a`s
  retrieveAll :: EntityCacheStore a -> IO [a]
  retrieveAll mvar = readMVar mvar >>= return . (snd <$>) . Map.toList . snd

  -- retrieve an `a` by name
  retrieveByName :: EntityCacheStore a -> String -> IO a
  retrieveByName mvar n =
    getIdByNameM mvar n >>= \case
      Nothing -> throw $ EntityNotFound $ show n
      Just i -> retrieve mvar i

  -- save an `a`
  save :: EntityCacheStore a -> Entity.EntityInput a -> IO a
  save mvar inp =
    getIdByNameM mvar (Entity.getName inp) >>= \case
      -- if name exists, throw error
      Just id' -> throw $ EntityAlreadyExists $ show id'
      Nothing -> do
        -- first, persist this new Entity; throw exception if err
        e <- Entity.save inp
        let (i, n) = (Entity.getId e, Entity.getName e)
        -- then, update the cache
        modifyMVar_ mvar $ \(mni, mie) ->
          return (Map.insert n i mni, Map.insert i e mie)
        return e

  -- update an `a`
  update :: EntityCacheStore a -> Id -> Entity.EntityInput a -> IO a
  update mvar i inp =
    readMVar mvar >>= \(_, m) ->
      case Map.lookup i m of
        Nothing -> throw $ EntityNotFound $ show i
        Just _ ->
          -- first persist, then update the cache
          Entity.update i inp >>= \e ->
            modifyMVar mvar $ \(mni, mie) ->
              let newMie = Map.insert i e mie
               in return ((mni, newMie), e)

  -- update an `a` by name
  updateByName :: EntityCacheStore a -> String -> Entity.EntityInput a -> IO a
  updateByName mvar n inp =
    readMVar mvar >>= \(m, _) ->
      case Map.lookup n m of
        Nothing -> throw $ EntityNotFound $ show n
        Just i ->
          -- first persist, then update the cache
          Entity.update i inp >>= \e ->
            modifyMVar mvar $ \(mni, mie) ->
              let newMie = Map.insert i e mie
               in return ((mni, newMie), e)

  -- remove an `a`
  remove :: EntityCacheStore a -> Id -> IO a
  remove mvar i =
    readMVar mvar >>= \(_, m) ->
      case Map.lookup i m of
        Nothing -> throw $ EntityNotFound $ show i
        Just _ ->
          -- first persist, then update the cache
          Entity.delete i >>= \e ->
            modifyMVar mvar $ \(mni, mie) ->
              let newMni = Map.delete (Entity.getName e) mni
                  newMie = Map.delete i mie
               in return ((newMni, newMie), e)

  -- remove an `a` by name
  removeByName :: EntityCacheStore a -> String -> IO a
  removeByName mvar n =
    readMVar mvar >>= \(m, _) ->
      case Map.lookup n m of
        Nothing -> throw $ EntityNotFound $ show n
        Just i ->
          -- first persist, then update the cache
          Entity.delete i >>= \e ->
            modifyMVar mvar $ \(mni, mie) ->
              let newM = (Map.delete n mni, Map.delete (Entity.getId e) mie)
               in return (newM, e)
