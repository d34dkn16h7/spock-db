{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DB where

import Model
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite as SQ

import qualified Data.Text as T

initDB = SQ.runSqlite "mydb.sqlite" $ do
	SQ.runMigration migrateAll

runDB action = SQ.runSqlite "mydb.sqlite" $ action

insertPerson p = SQ.insert $ p

newPID :: T.Text -> Int -> Int -> Int -> Leaderboard
newPID id s1 s2 s3 = Leaderboard id s1 s2 s3

getByID (pid :: T.Text) = selectList [LeaderboardName ==. pid] []--[LimitTo 1]

--extract :: [Entity] -> [Leaderboard]
extract [] = []
extract (e:ent) = eV : extract ent
	where
		eV = entityVal e

{-
extract (e:ent) = (newPID (leaderboardName eV) (leaderboardScoreEasy eV) (leaderboardScoreMedium eV) (leaderboardScoreHard eV) ) : extract ent
	where
		eV = entityVal e
-}