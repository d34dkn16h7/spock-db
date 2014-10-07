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
import Control.Monad.IO.Class (liftIO)

dPort = 3000

main = do
	DB.initDB
	spockT dPort id $ runUrl

-- fix <#> when subcomponent works
runUrl = do
		post "/newPlayer"                  $ auth newPlayer
		
		get  ("/get" <#> "/top")           $ Get.top
		get  ("/get" <#> "/ctop")          $ Get.cTop

		post ("/get" <#> "/rank")          $ auth Get.rank
		post ("/get" <#> "/crank")         $ auth Get.cRank

		post ("/get" <#> "/player")        $ auth Get.player
		post ("/get" <#> "/rplayer")       $ auth Get.rPlayer

		post ("/update" <#> "/name")       $ auth Update.name
		post ("/update" <#> "/score")      $ auth Update.score
		post ("/update" <#> "/cscore")     $ auth Update.cScore

		post ("/challenge" <#> "/new")     $ auth Challenge.new
		post ("/challenge" <#> "/set")     $ auth Challenge.set
		post ("/challenge" <#> "/get")     $ auth Challenge.get
		post ("/challenge" <#> "/getTo")   $ auth Challenge.getTo
		post ("/challenge" <#> "/getFrom") $ auth Challenge.getFrom
		post ("/challenge" <#> "/remove")  $ auth Challenge.remove

{- PAGES-}
mainPage = html $ (centerT . h1T) "Mail me : stanislavursache@outlook.com"

newPlayer = do
	(Just name :: Maybe Text) <- param k_playerName

	rawID <- liftIO $ DB.insertPerson (newPID name)
	liftIO $ DB.suId rawID
	json $ DB.keyOutLB rawID
