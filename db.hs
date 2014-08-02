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


initDB = SQ.runSqlite "mydb.sqlite" $ do
	SQ.runMigration migrateAll

runDB action = SQ.runSqlite "mydb.sqlite" $ action

insertPerson p = runDB $ SQ.insert $ p

newPerson :: Text -> Int -> Int -> Int -> Leaderboard
newPerson id s1 s2 s3 = Leaderboard id s1 s2 s3

getByID (pid :: Text) = runDB $ selectList [LeaderboardName ==. pid] []--[LimitTo 1]

updateScore' (pid :: Text) b (score :: Int) = updateWhere [LeaderboardName ==. pid] [b =. score]

extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e