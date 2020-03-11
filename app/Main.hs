{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main where

import Prelude hiding (lookup)
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Memcache
-- import qualified Data.HashTable.IO as H
import Control.Monad
import Control.Concurrent hiding (yield)
import Data.Word
import Control.Monad.Trans.Resource

import Network.Memcache.Op
import Network.Memcache.Response

-- import qualified Network.TLS as N
-- import Network.TLS.Extra.Cipher
-- import Data.X509.CertificateStore
-- import Data.Default.Class (def)
import qualified Database.Redis as R

import OpenSSL.EVP.Digest
import System.IO.Unsafe

import Data.Bits (Bits, shiftL, (.|.))
import Data.Word

sha1 :: Digest
sha1 = unsafePerformIO $ do
  (Just sha1') <- getDigestByName "sha1"
  return sha1'

digestOfValue :: BS.ByteString -> Word64
digestOfValue value = fromByteString $ BS.take 4 (digestBS sha1 value)

valueWithDigest :: BS.ByteString -> BS.ByteString
valueWithDigest value = BS.take 4 (digestBS sha1 value) <> value

splitDigest :: BS.ByteString -> (BS.ByteString,BS.ByteString)
splitDigest valueWithD = (BS.take 4 valueWithD, BS.drop 4 valueWithD)

fromByteString :: BS.ByteString -> Word64
fromByteString = B.foldl go 0
    where go acc i = (acc  `shiftL` 8) .|. (fromIntegral i)

main :: IO ()
main = do
  -- (Just certStore) <- readCertificateStore "azure-redis.crt"
  -- let tlsParams = (defaultParamsClient "localhost" "")
  --                 { clientSupported = def { supportedCiphers = ciphersuite_strong }
  --                 , clientShared = def { sharedCAStore = Nothign }
  --                 }
  -- let redisConnInfo = defaultConnectInfo
  --                     { connectHost = "localhost"
  --                     , connectPort = PortNumber 6380
  --                     , connectTLSParams = Just tlsParams
  --                     , connectAuth = Nothing }
  -- conn <- checkedConnect redisConnInfo
  conn <- R.checkedConnect R.defaultConnectInfo
  
  runTCPServer (serverSettings 11211 "*") $ \appData -> do
    runResourceT $ runConduit $ do
      (appSource appData)
        .| getOpText
        .| process conn
        .| putResponseText
        .| (appSink appData)

process :: R.Connection -> ConduitM (Either BS.ByteString Op) Response (ResourceT IO) ()
process ht = loop
  where
    loop :: ConduitM (Either t Op) Response (ResourceT IO) ()
    loop = do
      meOp <- await
      case meOp of
        Nothing -> return ()
        Just (Left _msg) -> yield Error >> loop
        Just (Right op) -> case op of
          SetOp key _flags _exptime _bytes value options -> do
            insert ht key $ value
            yield' options Stored
            loop
          CasOp key _flags _exptime _bytes version value options -> do
            ret <- liftIO $ R.runRedis ht $ do
              R.watch [key]
              mValue <- R.get key
              case mValue of
                Left _ -> do
                  R.unwatch
                  return NotFound
                Right Nothing -> do
                  R.unwatch
                  return NotFound
                Right (Just value') -> case digestOfValue value' == version of
                  True -> do
                    r <- R.multiExec $ do 
                      R.set key value
                    case r of
                      R.TxSuccess _ -> return Stored
                      R.TxAborted -> return Exists
                      R.TxError _ -> return Error
                  False ->
                    return Exists
            yield' options ret
            loop
          AddOp key _flags _exptime _bytes value options -> do
            -- r <- incr' True key value options
            -- case r of
            --   Left _ -> yield' options NotFound
            --   Right v -> yield' options $ Code (fromIntegral v)
            loop
          ReplaceOp key _flags _exptime _bytes value options -> do
            -- with $ \ht -> do
            --   mValue <- lookup ht key
            --   case mValue of
            --     Nothing -> yield' options NotStored
            --     Just (version', _value') -> do
            --       insert ht key (version' + 1, value)
            --       yield' options Stored
            loop
          AppendOp key flags exptime bytes value options -> append' True key flags exptime bytes value options >> loop
          PrependOp key flags exptime bytes value options -> append' False key flags exptime bytes value options >> loop
          GetOp keys -> processGet False keys >> loop
          GetsOp keys -> processGet True keys >> loop
          DeleteOp key options -> do
            delete ht key
            yield' options Deleted
            loop
          IncrOp key value options -> do
            r <- incr' True key value options
            case r of
              Left _ -> yield' options NotFound
              Right v -> yield' options $ Code (fromIntegral v)
            loop
          DecrOp key value options -> do
            r <- incr' False key value options
            case r of
              Left _ -> yield' options NotFound
              Right v -> yield' options $ Code (fromIntegral v)
            loop
          TouchOp key _exptime options -> do
            -- with $ \ht -> do
            --   mValue <- lookup ht key
            --   case mValue of
            --     Nothing -> yield' options NotFound
            --     Just (_, _value) -> yield' options Touched -- XXX
            loop
          PingOp -> yield Ok >> loop
          FlushAllOp -> do
            liftIO $ R.runRedis ht $ R.flushdb
            yield Ok
            loop
          VersionOp -> yield (Version "hemcached-0.0.1") >> loop
          QuitOp -> return ()
          StatsOp _args -> yield End >> loop

    incr' True key value options = liftIO $ R.runRedis ht $ do
      R.incrby key (fromIntegral value)
        
    incr' False key value options = liftIO $ R.runRedis ht $ do
      R.decrby key (fromIntegral value)

    append' isAppend key _flags _exptime _bytes value options = liftIO $ R.runRedis ht $ do
      R.append key value

    processGet _ [] = yield End
    processGet withVersion (key:rest) = do
      mValue <- lookup ht key
      case mValue of
        Right (Just value) -> do
          yield (Value key 0 (fromIntegral $ BS.length value) value (if withVersion then Just (digestOfValue value) else Nothing))
        Right Nothing -> return ()
        Left err -> return ()
      processGet withVersion rest

    yield' options resp = when (Noreply `notElem` options) $ yield resp
    
    delete ht key = liftIO $ R.runRedis ht $ do
      R.del [key]
    
    lookup ht key = liftIO $ R.runRedis ht $ do
      R.get key

    insert ht key value = liftIO $ R.runRedis ht $ do
      R.set key value
