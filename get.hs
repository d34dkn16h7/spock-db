{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Get where

import qualified DB
import qualified Web.Spock.Simple as SP

import Model
import Data.Text
import Control.Monad.IO.Class (liftIO)

player = do
	(Just pID :: Maybe Int) <- SP.param k_playerID

	out <- liftIO $ DB.getByID pID
	SP.json $ DB.extract out

rPlayer = do
	(Just pID :: Maybe Int) <- SP.param k_playerID

	out <- liftIO $ DB.getPlayerRandom pID
	SP.json $ (DB.extract out :: [Person])

top = do
	out <- liftIO $ DB.getTop
	SP.json $ DB.extract out

cTop = do
	out <- liftIO $ DB.getTopChallenge
	SP.json $ DB.extract out

rank = do 
	(Just score :: Maybe Int) <- SP.param k_score

	count <- liftIO $ DB.getPlayerRank $ score + 1
	SP.json $ [count :: Int]

cRank = do 
	(Just score :: Maybe Int) <- SP.param k_CScore

	count <- liftIO $ DB.getPlayerRankC $ score + 1
	SP.json $ [count :: Int]