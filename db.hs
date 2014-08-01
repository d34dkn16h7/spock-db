{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DB where

import Model
import qualified Database.Persist.Sqlite as SQ

initDB = SQ.runSqlite "mydb.sqlite" $ do
	SQ.runMigration migrateAll

runDB action = SQ.runSqlite "mydb.sqlite" $ action

insertPerson p s1 s2 s3 = SQ.insert $ Leaderboard p s1 s2 s3

person = Leaderboard "Stan" 5 15 45