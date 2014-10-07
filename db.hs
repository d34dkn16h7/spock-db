{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module DB where

import Model
import Data.Text hiding(count)
import Data.Aeson
import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource

{- --- -}

initDB = runSqlite "mydb.sqlite" $ runMigration migrateAll

runDB action = runSqlite "mydb.sqlite" $ action

{- Add -}

insertPerson person = runDB $ insert person

addMatch match = runDB $ insert match

{- Update -}

uMatchStat mid nStatus   = uDB (toKeyCP mid) CPoolStatus nStatus
uMatchTarget mid nStatus = uDB (toKeyCP mid) CPoolTargetScore nStatus

uScore pid score  = uDB (toKeyLB pid) PersonScore score
uCScore (pid :: Int) = runDB $ update (toKeyLB pid) [PersonChallengeScore +=. 3]
uName pid nName   = uDB (toKeyLB pid) PersonName nName

suId pid = uDB pid PersonDId (gInt $ keyOutLB pid)
muId pid = uDB pid CPoolDId (gInt $ keyOutCP pid)

{- Get -}

getByID (pid :: Int) = sList (PersonId ==. toKeyLB pid)

getMatchTo   (pid :: Int) = sList (CPoolTo   ==. pid)
getMatchFrom (pid :: Int) = sList (CPoolFrom ==. pid)
getByMatchID (pid :: Int) = sList (CPoolId   ==. toKeyCP pid)

getTop = runDB $ selectList [] [Desc PersonScore, LimitTo 10]
getTopChallenge = runDB $ selectList [] [Desc PersonChallengeScore, LimitTo 10]

getPlayerRank  (score :: Int) = runDB $ count [PersonScore >. score]
getPlayerRankC (score :: Int) = runDB $ count [PersonChallengeScore >. score]

getPlayerRandom w = (runDB $ rawSql (getWithout w) [])

{- Remove -}

removeMatch (mID :: Int) = runDB $ delete $ (toKeyCP mID :: CPoolId) 

{- Tools -}

uDB key wKey nVal = runDB $ update key [wKey =. nVal]

sList exp = runDB $ selectList [exp] []

gInt (SqlBackendKey k) = fromIntegral k

keyOutLB (PersonKey key) = key
keyOutCP (CPoolKey key) = key

toKeyLB key = (PersonKey $ fromIntegral key)
toKeyCP key = (CPoolKey $ fromIntegral key)

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e

getWithout w = 
	append (append "select ?? from Person where id!=" (pack $ show w)) " order by random() limit 1"