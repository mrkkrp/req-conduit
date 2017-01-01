--
-- Space-consumption benchmark for ‘req-conduit’.
--
-- Copyright © 2016–2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

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
