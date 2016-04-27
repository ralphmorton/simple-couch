{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Database.CouchDB.Types where

import Control.Lens (Lens')
import Control.Lens.TH
import Data.Aeson (FromJSON, ToJSON)

class (FromJSON e, ToJSON e) => Entity e where
    entityRev :: Lens' e (Maybe String)
    entityID :: Lens' e String

data CouchCfg = CouchCfg {
    _couchCfgBaseURL :: String,
    _couchCfgDB :: String
}

data KeySpec = NoKey | Key String | KeyRange String String | Keys [String] Bool deriving Show

data UUIDs = UUIDs [String] deriving Show

data CouchList e = CouchList [e]

data CouchListItem e = CouchListItem { cliValue :: e }

data CouchListItem' e = CouchListItem' { cliDoc :: e }

data CouchUpdateResult = CouchUpdateResult String

makeClassy ''CouchCfg
