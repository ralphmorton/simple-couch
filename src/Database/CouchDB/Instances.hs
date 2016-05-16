{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Instances() where

import Database.CouchDB.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.List (intersperse)
import Data.Text (Text)

--
-- Keyable instances
--

instance Keyable () where
    toKey _ = "{}"

instance Keyable String where
    toKey = show

instance Keyable ByteString where
    toKey = show

instance Keyable Text where
    toKey = show

instance Keyable Int where
    toKey = show

instance Keyable Double where
    toKey = show

instance Keyable Key where
    toKey (Key v) = toKey v

instance Keyable [Key] where
    toKey kx = "%5B" ++ contents ++ "%5D"
        where contents = concat . intersperse "," $ fmap toKey kx

--
-- JSON instances
--

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
