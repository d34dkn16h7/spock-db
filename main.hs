{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Model
import Web.Spock
import Data.Text
import Control.Monad.IO.Class (liftIO)

import qualified DB

dPort = 3000

main = do
	DB.initDB
	spockT dPort id $ runUrl

runUrl = do
		get  "/" 			  $ mainPage
		get  "/get/:pID"	  $ getPlayer
		post "/addPlayer"     $ addPlayer
		post "/pScore/easy"   $ updateScore LeaderboardScoreEasy
		post "/pScore/medium" $ updateScore LeaderboardScoreMedium
		post "/pScore/hard"   $ updateScore LeaderboardScoreHard

{- PAGES-}

mainPage = html $ "<center><h1> Hi There! Well, you should go. </h1></center>"

getPlayer = do
	(Just pID :: Maybe Text) <- param "pID"
	out <- liftIO $ DB.getByID pID
	json $ DB.extract out

addPlayer = do
	(Just id :: Maybe Text) <- param "pID"
	(Just s1 :: Maybe Int)  <- param "sEasy"
	(Just s2 :: Maybe Int)  <- param "sMedium"
	(Just s3 :: Maybe Int)  <- param "sHard"
	liftIO $ DB.insertPerson $ newPID id s1 s2 s3	
	jSucces

updateScore row = do 
			(Just id :: Maybe Text) <- param "pID"
			(Just score :: Maybe Int) <- param "nScore"
			liftIO $ DB.uScore id row score
			jSucces

emptyPage = text ""

jSucces = json $ ["succes" :: Text]
jFaid   = json $ ["fail" :: Text]

newPID :: Text -> Int -> Int -> Int -> Leaderboard
newPID id s1 s2 s3 = Leaderboard id s1 s2 s3
