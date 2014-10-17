{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Model

import qualified DB
import qualified Get
import qualified Update
import qualified Challenge

import Data.Text
import Web.Spock.Simple

dPort = 3000

main = do
	DB.initDB
	pool <- DB.createPool
	DB.lockDB pool

	spock dPort sessCfg DB.gConnFake (pool,"user","pass") runUrl
	where
		sessCfg = SessionCfg "def" (0) 42 id
	--spockT dPort id $ runUrl

runUrl = do
		get  "/"            $ mainPage
		post "/newPlayer"   $ auth newPlayer
		
		subcomponent "/get" $ do
			get  "/top"     $      Get.top
			get  "/ctop"    $      Get.cTop
			post "/rank"    $ auth Get.rank
			post "/crank"   $ auth Get.cRank
			post "/player"  $ auth Get.player
			post "/rplayer" $ auth Get.rPlayer

		subcomponent "/update" $ do
			post "/name"       $ auth Update.name
			post "/score"      $ auth Update.score
			post "/cscore"     $ auth Update.cScore

		subcomponent "/challenge" $ do
			post "/new"     	  $ auth Challenge.new
			post "/set"     	  $ auth Challenge.set
			post "/get"     	  $ auth Challenge.get
			post "/getTo"   	  $ auth Challenge.getTo
			post "/getFrom" 	  $ auth Challenge.getFrom
			post "/remove"  	  $ auth Challenge.remove

{- PAGES-}
mainPage = html $ (centerT . h1T) "Mail me : stanislavursache@outlook.com"

newPlayer = do
	(Just name :: Maybe Text) <- param k_playerName

	rawID <- DB.insertPerson (newPID name)
	DB.suId rawID
	json $ DB.keyOutLB rawID
