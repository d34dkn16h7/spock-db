{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Update where

import Model
import Data.Text
import Control.Monad.IO.Class (liftIO)

import qualified DB
import qualified Web.Spock.Simple as SP


name = do
	(Just pID :: Maybe Int) <- SP.param k_playerID
	(Just nName :: Maybe Text) <- SP.param k_playerName

	liftIO $ DB.uName pID nName
	jSucces

score = do 
	(Just pID :: Maybe Int) <- SP.param k_playerID
	(Just score :: Maybe Int) <- SP.param k_score

	liftIO $ DB.uScore pID score

	playerRank <- liftIO $ DB.getPlayerRank $ score + 1
	SP.json $ [playerRank :: Int]

cScore = do 
	(Just pID :: Maybe Int) <- SP.param k_playerID

	liftIO $ DB.uCScore pID