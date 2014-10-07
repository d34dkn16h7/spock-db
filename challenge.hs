{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Challenge where

import qualified DB
import qualified Web.Spock.Simple as SP

import Model
import Data.Text
import Control.Monad.IO.Class (liftIO)

new = do
	(Just from :: Maybe Int)        <- SP.param k_CPfrom
	(Just to :: Maybe Int)          <- SP.param k_CPto
	(Just targetScore :: Maybe Int) <- SP.param k_CPtarget
	(Just theme :: Maybe Int)       <- SP.param k_CPtheme

	rawID <- liftIO $ DB.addMatch $ newMatch from to targetScore theme
	liftIO $ DB.muId rawID
	SP.json $ DB.keyOutCP rawID

set = do
	(Just mID :: Maybe Int) <- SP.param k_CPmatchID
	(Just status :: Maybe Int) <- SP.param k_CPstatus
	(Just nTarget :: Maybe Int) <- SP.param k_CPtarget

	liftIO $ DB.uMatchStat mID status >> DB.uMatchTarget mID nTarget
	jSucces

get = do
	(Just dID :: Maybe Int) <- SP.param k_CPmatchID

	out <- liftIO $ DB.getByMatchID dID
	SP.json $ DB.extract out

getTo = do
	(Just to :: Maybe Int) <- SP.param k_CPto

	out <- liftIO $ DB.getMatchTo to
	SP.json $ DB.extract out

getFrom = do
	(Just from :: Maybe Int) <- SP.param k_CPfrom

	out <- liftIO $ DB.getMatchFrom from
	SP.json $ DB.extract out

remove = do
	(Just mID :: Maybe Int) <- SP.param k_CPmatchID

	liftIO $ DB.removeMatch mID
	jSucces