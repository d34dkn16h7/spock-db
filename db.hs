{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module DB where

import Model
import Data.Text
import Data.Aeson
import Database.Persist.Sqlite

{- --- -}

initDB = runSqlite "mydb.sqlite" $ runMigration migrateAll

runDB action = runSqlite "mydb.sqlite" $ action

insertPerson person = runDB $ insert person

getByID (pid :: Int) = runDB $ selectList [LeaderboardId ==. (Key $ toPersistValue pid)] [] --[LimitTo 1]

getTop sort = runDB $ selectList [] [Desc sort,LimitTo 10]

uScore pid row score = updateRowsByID (Key $ toPersistValue pid) row score

uName pid row nName = updateRowsByID (Key $ toPersistValue pid) row nName

updateRowsByName (pid :: Text) rowToUpdate nVal = 
	runDB $ updateWhere [LeaderboardName ==. pid] [rowToUpdate =. nVal]

updateRowsByID pid rowToUpdate nVal = 
	runDB $ update pid [rowToUpdate =. nVal]
	--runDB $ updateWhere [LeaderboardId ==. pid] [rowToUpdate =. nVal]


getKeyOutJ' rawID = unKey rawID
 

newPerson :: Text -> Int -> Int -> Int -> Leaderboard
newPerson id s1 s2 s3 = Leaderboard id s1 s2 s3

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e