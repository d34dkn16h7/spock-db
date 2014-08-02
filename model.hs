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
import Database.Persist.TH
import Control.Applicative
import Control.Monad

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
 toJSON (Leaderboard firstName lastName age likesPizza) =
    object [ "name"  	   .= firstName
           , "scoreEasy"   .= lastName
           , "scoreMedium" .= age
           , "scoreHard"   .= likesPizza
             ]