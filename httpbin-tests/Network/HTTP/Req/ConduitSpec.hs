{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Req.ConduitSpec
  ( spec )
where

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit ((=$=), runConduitRes, ConduitM)
import Data.Int (Int64)
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import System.IO (Handle)
import System.IO.Temp
import Test.Hspec
import qualified Data.ByteString     as B
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

spec :: Spec
spec = do

  describe "streaming 10 M request" $
    it "works" $ do
      let size :: Int64
          size = 10 * 1024 * 1024
          src = CL.replicate (10 * 1024) (B.replicate 1024 0)
      void (req POST (httpbin /: "post")
        (ReqBodySource size src) ignoreResponse mempty) :: IO ()

  describe "streaming 100 M response" $
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

instance MonadHttp IO where
  handleHttpException = throwIO

instance MonadHttp (ConduitM i o (ResourceT IO)) where
  handleHttpException = liftIO . throwIO

----------------------------------------------------------------------------
-- Helpers

-- | 'Url' representing <https://httpbin.org>.

httpbin :: Url 'Https
httpbin = https "httpbin.org"
