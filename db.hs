{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DB where

import Model
import Data.Text
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite as SQ
import Control.Monad.IO.Class  (liftIO)

{- --- -}

initDB = SQ.runSqlite "mydb.sqlite" $ SQ.runMigration migrateAll

runDB action = SQ.runSqlite "mydb.sqlite" $ action

insertPerson person = runDB $ SQ.insert person

getByID (pid :: Text) =
	runDB $ selectList [LeaderboardName ==. pid] [] --[LimitTo 1]

updateRow (pid :: Text) rowToUpdate nVal = 
	runDB $ updateWhere [LeaderboardName ==. pid] [rowToUpdate =. nVal]

newPerson :: Text -> Int -> Int -> Int -> Leaderboard
newPerson id s1 s2 s3 = Leaderboard id s1 s2 s3

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e