{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Instances() where

import Database.CouchDB.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson

instance FromJSON UUIDs where
    parseJSON (Object v) = UUIDs <$> v .: "uuids"
    parseJSON _ = mzero

instance FromJSON e => FromJSON (CouchList e) where
    parseJSON (Object v) = CouchList <$> v .: "rows"
    parseJSON _ = mzero

instance FromJSON e => FromJSON (CouchListItem e) where
    parseJSON (Object v) = CouchListItem <$> v .: "value"
    parseJSON _ = mzero

instance FromJSON e => FromJSON (CouchListItem' e) where
    parseJSON (Object v) = CouchListItem' <$> v .: "doc"
    parseJSON _ = mzero

instance FromJSON CouchUpdateResult where
    parseJSON (Object v) = CouchUpdateResult <$> v .: "rev"
    parseJSON _ = mzero
