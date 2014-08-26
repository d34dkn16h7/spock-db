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

getTop = runDB $ selectList [] [Desc LeaderboardScore,LimitTo 10]

uScore pid score = updateRowsByID (Key $ toPersistValue pid) LeaderboardScore score

uName pid nName = updateRowsByID (Key $ toPersistValue pid) LeaderboardName nName

updateRowsByName (pid :: Text) rowToUpdate nVal = 
	runDB $ updateWhere [LeaderboardName ==. pid] [rowToUpdate =. nVal]

updateRowsByID pid rowToUpdate nVal = 
	runDB $ update pid [rowToUpdate =. nVal]

getKeyOutJ' rawID = unKey rawID
 

newPerson :: Text -> Int -> Leaderboard
newPerson id s = Leaderboard id s

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e