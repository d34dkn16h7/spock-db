{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DB where

import Model
import Data.Text
import Database.Persist.Sqlite

{- --- -}

initDB = runSqlite "mydb.sqlite" $ runMigration migrateAll

runDB action = runSqlite "mydb.sqlite" $ action

insertPerson person = runDB $ insert person

getByID (pid :: Int) = runDB $ selectList [LeaderboardId ==. (Key $ toPersistValue pid)] [] --[LimitTo 1]

getKeyOut val = listToJSON [unKey val]

uScore pid row score = updateRowsByID (Key $ toPersistValue pid) row score

uName pid row nName = updateRowsByID (Key $ toPersistValue pid) row nName

updateRowsByName (pid :: Text) rowToUpdate nVal = 
	runDB $ updateWhere [LeaderboardName ==. pid] [rowToUpdate =. nVal]

updateRowsByID pid rowToUpdate nVal = 
	runDB $ updateWhere [LeaderboardId ==. pid] [rowToUpdate =. nVal]

newPerson :: Text -> Int -> Int -> Int -> Leaderboard
newPerson id s1 s2 s3 = Leaderboard id s1 s2 s3

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e