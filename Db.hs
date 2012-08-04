{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Db (
    couchTest
) where

--import Control.Monad.IO.Class (MonadIO, liftIO)
--import Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO)
--import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Generics (Data, Typeable)
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Generic

conn :: CouchConnection
conn = def {couchLogin = "admin", couchPass = "admin"}

data D = D { f1 :: Int, f2 :: String } deriving (Show, Data, Typeable)

--couchTest :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, MonadBaseControl IO m) => m ()
couchTest:: IO ()
couchTest = runCouch conn $ do
    rev1 <- couchPut "mydb" "my-doc1" "" [] $ D 123 "str"
    rev2 <- couchPut "mydb" "my-doc1" rev1 [] $ D 1234 "another"
    (rev3, d1 :: D) <- couchGet "mydb" "my-doc1" []
    liftIO $ print d1
    couchPut' "mydb" "my-doc1" [] $ D 12345 "third"    -- notice - no rev
    rev3 <- couchRev "mydb" "my-doc1"
    couchDelete "mydb" "my-doc1" rev3
