{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit ((=$=), runConduitRes, ConduitM)
import Data.Int (Int64)
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import System.IO.Temp
import Weigh
import qualified Data.ByteString     as B
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL

main :: IO ()
main = mainWith $ do
  io "streaming 5   M request  body" bigRequest  (5   * 1024 * 1024)
  io "streaming 25  M request  body" bigRequest  (25  * 1024 * 1024)
  io "streaming 50  M request  body" bigRequest  (50  * 1024 * 1024)
  io "streaming 100 M request  body" bigRequest  (100 * 1024 * 1024)
  io "streaming 5   M response body" bigResponse (5   * 1024 * 1024)
  io "streaming 25  M response body" bigResponse (25  * 1024 * 1024)
  io "streaming 50  M response body" bigResponse (50  * 1024 * 1024)
  io "streaming 100 M response body" bigResponse (100 * 1024 * 1024)

bigRequest :: Int64 -> IO ()
bigRequest size' = do
  let size  = (size' `quot` 1024) * 1024
      chunk = B.replicate 1024 0
  let src = CL.replicate (fromIntegral size `quot` 1024) chunk
  void $ req POST (httpbin /: "post")
    (ReqBodySource size src) ignoreResponse mempty

bigResponse :: Int -> IO ()
bigResponse size = withSystemTempFile "req-conduit" $ \_ h ->
  runConduitRes $
    req' GET (httpbin /: "stream-bytes" /~ size) NoReqBody
      httpSource mempty =$= CB.sinkHandle h

----------------------------------------------------------------------------
-- Instances

instance MonadHttp IO where
  handleHttpException = throwIO

instance MonadHttp (ConduitM i o (ResourceT IO)) where
  handleHttpException = liftIO . throwIO

----------------------------------------------------------------------------
-- Helpers

-- | 'Url' representing <https://httpbin.org>.

httpbin :: Url 'Https
httpbin = https "httpbin.org"
