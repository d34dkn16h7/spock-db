{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Get where

import qualified DB

import Model
import Web.Spock.Simple

player = do
	(Just pID :: Maybe Int) <- param k_playerID

	out <- DB.getByID pID
	json $ DB.extract out

rPlayer = do
	(Just pID :: Maybe Int) <- param k_playerID

	out <- DB.getPlayerRandom pID
	json $ (DB.extract out :: [Person])

top = do
	out <- DB.getTop
	json $ DB.extract out

cTop = do
	out <- DB.getTopChallenge
	json $ DB.extract out

rank = do 
	(Just score :: Maybe Int) <- param k_score

	count <- DB.getPlayerRank (score + 1)
	json $ [count :: Int]

cRank = do 
	(Just score :: Maybe Int) <- param k_CScore

	count <- DB.getPlayerRankC (score + 1)
	json $ [count :: Int]