{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module DB where

import Model
import Database.Persist.Sqlite

import Data.Text hiding (count)
import Web.Spock.Simple hiding (delete)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)

{- --- -}

dbName = "mydb.sqlite"

gConnFake = PCConn (ConnBuilder (return ()) (const (return ())) (PoolCfg 1 1 1))

initDB     = runSqlite dbName $ runMigration migrateAll
createPool = runNoLoggingT $ createSqlitePool dbName 10
lockDB p   = runSqlPool (selectFirst [PersonId ==. (toKeyP (0 :: Int))] []) p

runIOPool action = do
	(pool,_,_) <- getState
	liftIO $ runSqlPool action pool

sList exp         = runIOPool $ selectList [exp] []
uDB key wKey nVal = runIOPool $ update key [wKey =. nVal]

{- Add -}

insertPerson person = runIOPool (insert person)
addMatch match      = runIOPool (insert match)

{- Update -}

uMatchStat mid nStatus   = uDB (toKeyCP mid) CPoolStatus nStatus
uMatchTarget mid nStatus = uDB (toKeyCP mid) CPoolTargetScore nStatus

uCScore pid       = runIOPool $ update (toKeyP pid) [PersonChallengeScore +=. 3]
uScore pid score  = uDB (toKeyP pid) PersonScore score
uName pid nName   = uDB (toKeyP pid) PersonName nName

suId pid = uDB pid PersonDId (gInt $ keyOutLB pid)
muId pid = uDB pid CPoolDId  (gInt $ keyOutCP pid)

{- Get -}

getByID      (pid :: Int) = sList (PersonId  ==. toKeyP pid)
getByMatchID (pid :: Int) = sList (CPoolId   ==. toKeyCP pid)
getMatchTo   (pid :: Int) = sList (CPoolTo   ==. pid)
getMatchFrom (pid :: Int) = sList (CPoolFrom ==. pid)

getTop          = runIOPool (selectList [] [Desc PersonScore, LimitTo 10])
getTopChallenge = runIOPool (selectList [] [Desc PersonChallengeScore, LimitTo 10])

getPlayerRank (score :: Int) = runIOPool (count [PersonScore >. score])
getPlayerRankC (score :: Int) = runIOPool (count [PersonChallengeScore >. score])

getPlayerRandom w = runIOPool (rawSql (getWithout w) [])

{- Remove -}

removeMatch (mID :: Int) = runIOPool $ delete $ (toKeyCP mID :: CPoolId) 

{- Tools -}
gInt (SqlBackendKey k) = fromIntegral k

keyOutLB (PersonKey key) = key
keyOutCP (CPoolKey key) = key

toKeyP key = (PersonKey $ fromIntegral key)
toKeyCP key = (CPoolKey $ fromIntegral key)

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e

getWithout w = 
	append (append "select ?? from Person where id!=" (pack $ show w)) " order by random() limit 1"