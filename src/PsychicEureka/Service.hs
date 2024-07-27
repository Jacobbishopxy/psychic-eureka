-- file: Service.hs
-- author: Jacob Xie
-- date: 2024/07/27 19:28:46 Saturday
-- brief:

module PsychicEureka.Service where

import qualified PsychicEureka.Cache as Cache

class (Cache.EntityCache a) => EntityService a

-- TODO
