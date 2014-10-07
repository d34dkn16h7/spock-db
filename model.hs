{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Model where

import qualified Web.Spock.Simple as SP

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Applicative
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  dId Int
  name Text
  score Int
  challengeScore Int
  deriving Show
CPool
  dId Int
	from Int
	to Int
	targetScore Int
  theme Int
  status Int
|]

{- JSON -}

instance FromJSON Person where
 parseJSON (Object v) =
    Person <$> v .: "dId"
                <*> v .: "name"
                <*> v .: "score"
                <*> v .: "challengeScore"
 parseJSON _ = mzero

instance ToJSON Person where
 toJSON (Person dId name score challengeScore) =
    object [ "dId"            .= dId
           , "name"           .= name
           , "score"          .= score 
           , "challengeScore" .= challengeScore ]

instance FromJSON CPool where
 parseJSON (Object v) =
  CPool <$> v .: "dId"
        <*> v .: "from"
        <*> v .: "to"
        <*> v .: "targetScore"
        <*> v .: "theme"
        <*> v .: "status"
 parseJSON _ = mzero

instance ToJSON CPool where
 toJSON (CPool dId from to targetScore theme status) =
    object [ "dId"         .= dId
           , "from"        .= from
           , "to"          .= to
           , "targetScore" .= targetScore
           , "theme"       .= theme
           , "status"      .= status]

{- Keys-}

k_score      = "nScore" :: Text
k_CScore     = "cScore" :: Text
k_playerID   = "pID" :: Text
k_playerName = "name" :: Text

k_CPmatchID = "mID" :: Text
k_CPfrom    = "from" :: Text
k_CPto      = "to" :: Text
k_CPtarget  = "targetScore" :: Text
k_CPtheme   = "theme" :: Text
k_CPstatus  = "status" :: Text

ce_newMatch   = 0 :: Int
ce_senderWin  = 1 :: Int
ce_senderLose = 2 :: Int

{- Tools -}

jSucces = SP.json $ ["succes" :: Text]

newPID name = Person (-1) name 0 0

newMatch f t tS th = CPool (-1) f t tS th ce_newMatch

centerT t = wrapT "<center>" "</center>" t
h1T t = wrapT"<h1>" "</h1>" t

wrapT b e t = append (append b t) e

canLogin user pass = return (user == "test" && pass == "pass")

auth action = SP.requireBasicAuth "AUTH" canLogin $ action