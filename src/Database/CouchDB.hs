{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB(
    Database.CouchDB.Types.CouchCfg(..),
    Database.CouchDB.Types.HasCouchCfg(..),
    Database.CouchDB.Types.Entity(..),
    Database.CouchDB.Types.KeySpec(..),
    uuids,
    view,
    view',
    get,
    getMany,
    getMaybe,
    create,
    update,
    delete
) where

import Database.CouchDB.Types
import Database.CouchDB.Instances

import Control.Lens ((&), (.~), (^.), preview, set)
import qualified Control.Lens as L
import Control.Monad (mzero)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.List (intersperse)
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Nano
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Types.URI (urlEncode)

-- |Generate UUIDs
uuids :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m) => Int -> m [String]
uuids n = do
    url <- L.view couchCfgBaseURL >>= return . (++ ("_uuids?count=" ++ show n))
    req <- buildReq GET url NoRequestData
    httpJSON req >>= return . unUUIDs
    where unUUIDs (UUIDs sx) = sx

-- |Query a view, returning row values
view :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m, FromJSON a) => String -> String -> KeySpec -> m [a]
view docName viewName ks = fmap (fmap cliValue) $ view' docName viewName ks

-- |Query a view, returning rows
view' :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m, FromJSON a) => String -> String -> KeySpec -> m [a]
view' docName viewName ks = return . concat =<< mapM listInt (splitKey ks)
    where
    listInt ks' = do
        url <- viewURL docName viewName ks'
        req <- buildReq GET url NoRequestData
        (CouchList rows) <- httpJSON req
        return rows

-- |Get a doc by ID
get :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m, FromJSON a) => String -> m a
get eid = do
    url <- docURL eid
    req <- buildReq GET url NoRequestData
    httpJSON req

-- |Get docs by their IDs
getMany :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m, FromJSON a) => [String] -> m [a]
getMany eids = do
    url <- return . (++"_all_docs?include_docs=true") =<< dbURL
    req <- buildReq POST url $ mkJSONData (object ["keys" .= eids])
    (CouchList rows) <- httpJSON req
    return $ fmap cliDoc rows

-- |Get a doc by ID (if such a doc exists)
getMaybe :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m, FromJSON a) => String -> m (Maybe a)
getMaybe eid
    | length eid == 0 = return Nothing
    | otherwise = flip catchError stifle404 $ fmap Just (get eid)
    where
    stifle404 e = case (preview _HttpError e) of
        Nothing -> throwError e
        Just (NetworkError (StatusCodeException (Status 404 _) _ _)) -> return Nothing
        Just _ -> throwError e

-- |Create a document (a server-generated UUID will be set as doc _id)
create :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m, Entity a) => a -> m a
create e = do
    uuid <- fmap head $ uuids 1
    let e' = set entityID uuid e
    update e'

-- |Update a document. Can be used to create a document if a specific _id is required
update :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m, Entity a) => a -> m a
update e = do
    url <- docURL (L.view entityID e)
    req <- buildReq PUT url (mkJSONData e)
    (CouchUpdateResult rev) <- httpJSON req
    return $ e & entityRev .~ (Just rev)

-- |Delete a doc with given ID and Rev
delete :: (MonadReader r m, MonadError e m, HasCouchCfg r, HasHttpCfg r, AsHttpError e, MonadIO m) => String -> String -> m ()
delete eid rev = do
    url <- dbURL >>= return . (++ (eid ++ "?rev=" ++ rev))
    req <- buildReq DELETE url NoRequestData
    http' req

---
---
---

splitKey :: KeySpec -> [KeySpec]
splitKey (Keys [] _) = []
splitKey (Keys kx group) = Keys px group : splitKey (Keys sx group) where (px, sx) = splitAt 40 kx
splitKey ks = [ks]

dbURL :: (MonadReader r m, HasCouchCfg r) => m String
dbURL = do
    burl <- L.view couchCfgBaseURL
    db <- L.view couchCfgDB
    return $ burl ++ db ++ "/"

docURL :: (MonadReader r m, HasCouchCfg r) => String -> m String
docURL eid = dbURL >>= return . (++ eid)

viewURL :: (MonadReader r m, HasCouchCfg r) => String -> String -> KeySpec -> m String
viewURL docName viewName ks = do
    url <- dbURL
    return $ url ++ "_design/" ++ docName ++ "/_view/" ++ viewName ++ keyStr ks

keyStr :: KeySpec -> String
keyStr NoKey = ""
keyStr (Key s) = "?key=" ++ s
keyStr (KeyRange s e) = "?startkey=" ++ s ++ "&endkey=" ++ e
keyStr (Keys sx group)
    | group = base ++"&group=true"
    | otherwise = base
    where base = "?keys=%5B" ++ (concat $ intersperse "," sx) ++ "%5D"
