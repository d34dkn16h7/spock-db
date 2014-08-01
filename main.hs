{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Spock
import qualified DB
import Control.Monad.IO.Class
import qualified Data.Text as T

dPort = 3000

main = do
	db <- DB.connectDB
	putStrLn "To exit press [CTRL + C]"
	spockT dPort id $ runUrl db
	DB.closeDB db

runUrl db = do 
		get "/" $ mainPage
		get "/get/:id" $ emptyPage

mainPage = text $ T.concat ["Echo: ", "hi!"]

emptyPage = text ""


	{-
		S.get "/get/:id" $ do
			(findID :: Int) <- S.param "id"
			--liftIO $ DB.getLeaderboard db >>= DB.printRows
			a <-liftIO $ DB.getLeaderboard db
			liftIO $ print (toAeson (a !! 0))
	
			S.text "Text"
	
		S.get "/post/:id/:score" $ do
			(id :: Int) <- S.param "id"
			(score :: Int) <- S.param "score"
			liftIO $ DB.insertScore db id score
			S.redirect "/"
	
		S.matchAny "/"   $ S.html "<center><h1>Hi There!</h1></center>"
	-}