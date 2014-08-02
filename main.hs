{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Web.Spock
import Model
import qualified DB
import Control.Monad.IO.Class
import qualified Data.Text as T

import qualified Database.Persist.Sqlite as SQ

dPort = 3000

main = do
	DB.initDB
	putStrLn "To exit press [CTRL + C]"
	spockT dPort id $ runUrl

runUrl = do
		get "/" mainPage
		get "/get/:pID" getPlayer
		post "/postScore" postScore

--------------------------------------------

mainPage = html $ "<center><h1> Hi There! Well, you should go. </h1></center>"

getPlayer = do
	(Just pID :: Maybe T.Text) <- param "pID"
	a <- liftIO $ DB.runDB $ DB.getByID pID
	b <- liftIO $ print $ DB.extract a
	json $ DB.extract a

postScore = do
	(Just id :: Maybe T.Text) <- param "pID"
	(Just s1 :: Maybe Int)    <- param "sE"
	(Just s2 :: Maybe Int)    <- param "sM"
	(Just s3 :: Maybe Int)    <- param "sH"
	addPlayer $ newPID id s1 s2 s3	
	jSucces

addPlayer player = liftIO $ DB.runDB $ DB.insertPerson player

emptyPage = text ""

jSucces = json $ ["done" :: T.Text]

newPID :: T.Text -> Int -> Int -> Int -> Leaderboard
newPID id s1 s2 s3 = Leaderboard id s1 s2 s3