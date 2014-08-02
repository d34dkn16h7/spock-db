{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Applicative
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Leaderboard
  name Text
  scoreEasy Int
  scoreMedium Int
  scoreHard Int
  deriving Show
|]

instance FromJSON Leaderboard where
 parseJSON (Object v) =
    Leaderboard <$> v .: "name"
	            <*> v .: "scoreEasy"
	            <*> v .: "scoreMedium"
	            <*> v .: "scoreHard"
 parseJSON _ = mzero

instance ToJSON Leaderboard where
 toJSON (Leaderboard name sEasy sMedium sHard) =
    object [ "name"  	   .= name
           , "scoreEasy"   .= sEasy
           , "scoreMedium" .= sMedium
           , "scoreHard"   .= sHard
           ]