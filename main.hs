{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Web.Spock
import qualified DB
import Control.Monad.IO.Class
import qualified Data.Text as T

dPort = 3000

main = do
	DB.initDB
	putStrLn "To exit press [CTRL + C]"
	spockT dPort id $ runUrl

runUrl = do 
		get "/" mainPage
		get "/get" emptyPage
		post "/postScore" $ do 
									(Just id :: Maybe T.Text) <- param "id"
									(Just s1 :: Maybe Int) <- param "scoreE"
									(Just s2 :: Maybe Int) <- param "scoreM"
									(Just s3 :: Maybe Int) <- param "scoreH"
									postScore id s1 s2 s3

--------------------------------------------

mainPage = html $ "<center><h1> Hi There! </h1></center>"

postScore :: MonadIO m => T.Text -> Int -> Int -> Int -> ActionT m ()
postScore id s1 s2 s3 = do
			liftIO $ DB.runDB $ DB.insertPerson id s1 s2 s3
			text ""

emptyPage = text ""