{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Challenge where

import qualified DB

import Model
import Web.Spock.Simple

new = do
	(Just from :: Maybe Int)        <- param k_CPfrom
	(Just to :: Maybe Int)          <- param k_CPto
	(Just targetScore :: Maybe Int) <- param k_CPtarget
	(Just theme :: Maybe Int)       <- param k_CPtheme

	rawID <- DB.addMatch $ newMatch from to targetScore theme
	DB.muId rawID
	json $ DB.keyOutCP rawID

set = do
	(Just mID :: Maybe Int) <- param k_CPmatchID
	(Just status :: Maybe Int) <- param k_CPstatus
	(Just nTarget :: Maybe Int) <- param k_CPtarget

	DB.uMatchStat mID status
	DB.uMatchTarget mID nTarget

	jSucces

get = do
	(Just dID :: Maybe Int) <- param k_CPmatchID

	out <- DB.getByMatchID dID
	json $ DB.extract out

getTo = do
	(Just to :: Maybe Int) <- param k_CPto

	out <- DB.getMatchTo to
	json $ DB.extract out

getFrom = do
	(Just from :: Maybe Int) <- param k_CPfrom

	out <- DB.getMatchFrom from
	json $ DB.extract out

remove = do
	(Just mID :: Maybe Int) <- param k_CPmatchID

	DB.removeMatch mID
	jSucces