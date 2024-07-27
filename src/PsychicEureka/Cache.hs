{-# LANGUAGE LambdaCase #-}

-- file: Cache.hs
-- author: Jacob Xie
-- date: 2024/07/27 15:58:21 Saturday
-- brief:

module PsychicEureka.Cache where

import Control.Exception (throw)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import qualified Data.Map as Map
import qualified PsychicEureka.Entity as Entity
import PsychicEureka.Error
import PsychicEureka.Util (Id)

----------------------------------------------------------------------------------------------------
-- EntityCache
----------------------------------------------------------------------------------------------------

type EntityCacheStore a = IORef (Map.Map String Id, Map.Map Id a)

class (Entity.Entity a) => EntityCache a where
  -- init
  initialize :: IO (EntityCacheStore a)
  initialize =
    Entity.retrieveAll >>= \a ->
      let m1 = Map.fromList [(Entity.getName e, Entity.getId e) | e <- a]
          m2 = Map.fromList [(Entity.getId e, e) | e <- a]
       in newIORef (m1, m2)

  -- get name id mapping
  getNameMap :: EntityCacheStore a -> IO (Map.Map String Id)
  getNameMap ref = readIORef ref >>= return . fst

  -- get id by name
  getIdByName :: EntityCacheStore a -> String -> IO (Maybe Id)
  getIdByName ref n = readIORef ref >>= return . Map.lookup n . fst

  -- retrieve an `a` from a caching
  retrieve :: EntityCacheStore a -> Id -> IO a
  retrieve ref i =
    readIORef ref >>= \(_, m) ->
      case Map.lookup i m of
        Nothing -> throw $ EntityNotFound $ show i
        Just e -> return e

  -- retrieve all `a`s
  retrieveAll :: EntityCacheStore a -> IO [a]
  retrieveAll ref = readIORef ref >>= return . (snd <$>) . Map.toList . snd

  -- retrieve an `a` by name
  retrieveByName :: EntityCacheStore a -> String -> IO a
  retrieveByName ref n =
    getIdByName ref n >>= \case
      Nothing -> throw $ EntityNotFound $ show n
      Just i -> retrieve ref i

  -- save an `a`
  save :: EntityCacheStore a -> Entity.EntityInput a -> IO a
  save ref inp =
    getIdByName ref (Entity.getName inp) >>= \case
      -- if name exists, throw error
      Just id' -> throw $ EntityAlreadyExists $ show id'
      Nothing -> do
        -- first, persist this new Entity; throw exception if err
        e <- Entity.save inp
        let i = Entity.getId e
            n = Entity.getName e
        -- then, update the cache
        _ <- atomicModifyIORef ref $ \(mni, mie) ->
          let newMni = Map.insert n i mni
              newMie = Map.insert i e mie
           in ((newMni, newMie), ())
        return e

  -- update an `a`
  update :: EntityCacheStore a -> Id -> Entity.EntityInput a -> IO a
  update ref i inp =
    readIORef ref >>= \(_, m) ->
      case Map.lookup i m of
        Nothing -> throw $ EntityNotFound $ show i
        Just _ ->
          -- first persist, then update the cache
          Entity.update i inp >>= \e ->
            atomicModifyIORef ref $ \(mni, mie) ->
              let newMie = Map.insert i e mie
               in ((mni, newMie), e)

  -- update an `a` by name
  updateByName :: EntityCacheStore a -> String -> Entity.EntityInput a -> IO a
  updateByName ref n inp =
    readIORef ref >>= \(m, _) ->
      case Map.lookup n m of
        Nothing -> throw $ EntityNotFound $ show n
        Just i ->
          -- first persist, then update the cache
          Entity.update i inp >>= \e ->
            atomicModifyIORef ref $ \(mni, mie) ->
              let newMie = Map.insert i e mie
               in ((mni, newMie), e)

  -- delete an `a`
  delete :: EntityCacheStore a -> Id -> IO a
  delete ref i =
    readIORef ref >>= \(_, m) ->
      case Map.lookup i m of
        Nothing -> throw $ EntityNotFound $ show i
        Just _ ->
          Entity.delete i >>= \e ->
            atomicModifyIORef ref $ \(mni, mie) ->
              let newMni = Map.delete (Entity.getName e) mni
                  newMie = Map.delete i mie
               in ((newMni, newMie), e)

  -- delete an `a` by name
  deleteByName :: EntityCacheStore a -> String -> IO a
  deleteByName ref n =
    readIORef ref >>= \(m, _) ->
      case Map.lookup n m of
        Nothing -> throw $ EntityNotFound $ show n
        Just i ->
          Entity.delete i >>= \e ->
            atomicModifyIORef ref $ \(mni, mie) ->
              let newMni = Map.delete n mni
                  newMie = Map.delete (Entity.getId e) mie
               in ((newMni, newMie), e)
