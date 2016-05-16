{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.CouchDB.Types where

import Control.Lens (Lens')
import Control.Lens.TH
import Data.Aeson (FromJSON, ToJSON)

class (FromJSON e, ToJSON e) => Entity e where
    entityRev :: Lens' e (Maybe String)
    entityID :: Lens' e String

class Keyable k where
    toKey :: k -> String

data CouchCfg = CouchCfg {
    _couchCfgBaseURL :: String,
    _couchCfgDB :: String
}

data KeySpec = NoKey | SingleKey Key | KeyRange Key Key | Keys [Key] Bool

data Key = forall k. Keyable k => Key k

data UUIDs = UUIDs [String] deriving Show

data CouchList e = CouchList [e]

data CouchListItem e = CouchListItem { cliValue :: e }

data CouchListItem' e = CouchListItem' { cliDoc :: e }

data CouchUpdateResult = CouchUpdateResult String

makeClassy ''CouchCfg
