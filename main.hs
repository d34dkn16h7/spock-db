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
		get  "/" 		  $ mainPage
		get  "/get/:pID"  $ getPlayer
		get  "/getTop"    $ getTop

		post "/addPlayer" $ addPlayer
		post "/newName"   $ updateName
		post "/pScore"    $ updateScore
		post "/getRank"   $ getRank

{- PAGES-}

mainPage = 
	html $ "<center><h1> Hi There!\nYou shouldn't be here. This is a server for Leaderboard and stuff. Got anything to ask? \n Mail me : d34dkn16h7@gmail.com </h1></center>"


getPlayer = do
	(Just id :: Maybe Int) <- param "pID"
	out <- liftIO $ DB.getByID id
	json $ DB.extract out

getTop = do
	out <- liftIO $ DB.getTop
	json $ DB.extract out

getRank = do 
	(Just score :: Maybe Int) <- param "nScore"
	count <- liftIO $ DB.getPlayerRank score
	json $ [count :: Int]

addPlayer = do
	(Just name :: Maybe Text) <- param "name"
	rawID <- liftIO $ DB.insertPerson $ newPID name
	json $ DB.getKeyOutJ' rawID

updateName = do
	(Just id :: Maybe Int) <- param "pID"
	(Just nName :: Maybe Text) <- param "name"
	liftIO $ DB.uName id nName
	jSucces

updateScore = do 
	(Just id :: Maybe Int) <- param "pID"
	(Just score :: Maybe Int) <- param "nScore"
	liftIO $ DB.uScore id score
	jSucces

emptyPage = text ""

jSucces = json $ ["succes" :: Text]
jFaid   = json $ ["fail" :: Text]

newPID :: Text -> Leaderboard
newPID id = Leaderboard id 0
