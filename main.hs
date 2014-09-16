{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Model
import Web.Spock
import Data.Text
import System.Random
import Control.Monad.IO.Class (liftIO)

import qualified DB

dPort = 3000

k_score      = "nScore"
k_CScore     = "cScore"
k_playerID   = "pID"
k_playerName = "name"

k_CPmatchID = "mID"
k_CPfrom    = "from"
k_CPto      = "to"
k_CPtarget  = "targetScore"
k_CPtheme   = "theme"
k_CPstatus  = "status"

ce_newMatch   = 0
ce_senderWin  = 1
ce_senderLose = 2

main = do
	DB.initDB
	spockT dPort id $ runUrl

runUrl = do
		get  "/" 		  $ mainPage
		get  "/get/:pID"  $ getPlayer
		get  "/getTop"    $ getTop
		get  "/getTopC"   $ getTopC

		get  "/rUser"     $ getRandom

		post "/addPlayer" $ addPlayer
		post "/newName"   $ updateName
		post "/pScore"    $ updateScore
		post "/getRank"   $ getRank
		post "/getRankC"  $ getRankC

		post "/newChallenge"    $ newChallenge
		post "/getChallenge"    $ getChallenge
		post "/setChallenge"    $ setChallenge
		post "/removeChallenge" $ removeChallenge

{- PAGES-}

mainPage = 
	html $ "<center><h1> Got anything to ask? \n Mail me : stanislavursache@outlook.com </h1></center>"

getRandom = do
	out <- liftIO $ DB.getPlayerRandom
	json $ (DB.extract out :: [Leaderboard]) --  :: [Int]

getPlayer = do
	(Just pID :: Maybe Int) <- param k_playerID

	out <- liftIO $ DB.getByID pID
	json $ DB.extract out

getTop = do
	out <- liftIO $ DB.getTop
	json $ DB.extract out

getTopC = do
	out <- liftIO $ DB.getTopChallenge
	json $ DB.extract out

getRank = do 
	(Just score :: Maybe Int) <- param k_score

	count <- liftIO $ DB.getPlayerRank $ score + 1
	json $ [count :: Int]

getRankC = do 
	(Just score :: Maybe Int) <- param k_CScore

	count <- liftIO $ DB.getPlayerRankC $ score + 1
	json $ [count :: Int]

addPlayer = do
	(Just name :: Maybe Text) <- param k_playerName

	rawID <- liftIO $ DB.insertPerson $ newPID name
	liftIO $ DB.suId rawID
	json $ DB.keyOut rawID

updateName = do
	(Just pID :: Maybe Int) <- param k_playerID
	(Just nName :: Maybe Text) <- param k_playerName

	liftIO $ DB.uName pID nName
	jSucces

updateScore = do 
	(Just pID :: Maybe Int) <- param k_playerID
	(Just score :: Maybe Int) <- param k_score

	liftIO $ DB.uScore pID score

	playerRank <- liftIO $ DB.getPlayerRank $ score + 1
	json $ [playerRank :: Int]

newChallenge = do
	(Just from :: Maybe Int)        <- param k_CPfrom
	(Just to :: Maybe Int)          <- param k_CPto
	(Just targetScore :: Maybe Int) <- param k_CPtarget
	(Just theme :: Maybe Int)       <- param k_CPtheme

	rawID <- liftIO $ DB.addMatch $ newMatch from to targetScore theme
	liftIO $ DB.muId rawID
	json $ DB.keyOut rawID

getChallenge = do
	(Just to :: Maybe Int) <- param k_CPto

	out <- liftIO $ DB.getMatch to
	json $ DB.extract out

setChallenge = do
	(Just mID :: Maybe Int) <- param k_CPmatchID
	(Just status :: Maybe Int) <- param k_CPstatus
	(Just nTarget :: Maybe Int) <- param k_CPtarget

	liftIO $ DB.uMatchStat mID status
	liftIO $ DB.uMatchTarget mID nTarget
	jSucces

removeChallenge = do
	(Just mID :: Maybe Int) <- param k_CPmatchID

	liftIO $ DB.removeMatch mID
	jSucces

{- Tools -}

jSucces = json $ ["succes" :: Text]

newPID name = Leaderboard (-1) name 0 0

newMatch f t tS th = CPool (-1) f t tS th ce_newMatch