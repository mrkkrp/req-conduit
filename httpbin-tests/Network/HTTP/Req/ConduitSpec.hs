--
-- Tests for ‘req-conduit’ package. This test suite tests streaming large
-- request and response bodies.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
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

module Network.HTTP.Req.ConduitSpec
  ( spec )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit ((=$=), runConduitRes, ConduitM)
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import System.IO (Handle)
import System.IO.Temp
import Test.Hspec
import qualified Data.Conduit.Binary as CB

spec :: Spec
spec = do

  describe "streaming 100 Mb of data" $
    it "works" $ do
      let tempi :: (Handle -> IO ()) -> IO ()
          tempi f = withSystemTempFile "req-conduit" (const f)
      tempi $ \h ->
        runConduitRes $ do
          let size :: Int
              size = 100 * 1024 * 1024
          req' GET (httpbin /: "stream-bytes" /~ size) NoReqBody
            httpSource mempty =$= CB.sinkHandle h

----------------------------------------------------------------------------
-- Instances

instance MonadHttp (ConduitM i o (ResourceT IO)) where
  handleHttpException = liftIO . throwIO

----------------------------------------------------------------------------
-- Helpers

-- | 'Url' representing <https://httpbin.org>.

httpbin :: Url 'Https
httpbin = https "httpbin.org"
