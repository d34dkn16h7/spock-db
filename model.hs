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

data UserID = UserID { uID :: Int}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Leaderboard
  name Text
  score Int
  deriving Show
CPool
	from Int
	to Int
	target Int
|]

instance FromJSON Leaderboard where
 parseJSON (Object v) =
    Leaderboard <$> v .: "name"
	            <*> v .: "score"
 parseJSON _ = mzero

instance ToJSON Leaderboard where
 toJSON (Leaderboard name score) =
    object [ "name"  .= name
           , "score" .= score ]


instance FromJSON UserID where
 parseJSON (Object v) = UserID <$> v .: "uID"
 parseJSON _ = mzero

instance ToJSON UserID where
 toJSON (UserID uid) =
    object [ "uID" .= uid]
