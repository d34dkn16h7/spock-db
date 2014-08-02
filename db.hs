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

getByID (pid :: Text) =
	runDB $ selectList [LeaderboardName ==. pid] [] --[LimitTo 1]

uScore pid row score = updateRow pid row score

updateRow (pid :: Text) rowToUpdate nVal = 
	runDB $ updateWhere [LeaderboardName ==. pid] [rowToUpdate =. nVal]

newPerson :: Text -> Int -> Int -> Int -> Leaderboard
newPerson id s1 s2 s3 = Leaderboard id s1 s2 s3

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e