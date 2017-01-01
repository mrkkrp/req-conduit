-- |
-- Module      :  Network.HTTP.Req.Conduit
-- Copyright   :  © 2016–2017 Mark Karpov, Michael Snoyman
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module extends functionality available in "Network.HTTP.Req" with
-- Conduit helpers for streaming big request bodies.
--
-- The package re-uses some pieces of code from the @http-conduit@ package,
-- but not to the extent that depending on that package is reasonable.

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ <  710
{-# LANGUAGE ConstraintKinds #-}
#endif

module Network.HTTP.Req.Conduit
  ( -- * Streaming request bodies
    ReqBodySource (..)
    -- * Streaming response bodies
    -- $streaming-response
  , req'
  , httpSource )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource (..))
import Data.ByteString (ByteString)
import Data.Conduit (Source, ($$+), ($$++), await, yield)
import Data.IORef
import Data.Int (Int64)
import Network.HTTP.Req
import qualified Data.ByteString     as B
import qualified Data.Conduit        as C
import qualified Network.HTTP.Client as L

----------------------------------------------------------------------------
-- Request bodies

-- | This body option streams contents of request body from given
-- 'C.Source'. The 'Int64' value is size of the data in bytes.
--
-- Using of this body option does not set the @Content-Type@ header.

data ReqBodySource = ReqBodySource Int64 (C.Source IO ByteString)

instance HttpBody ReqBodySource where
  getRequestBody (ReqBodySource size src) =
    L.RequestBodyStream size (srcToPopperIO src)

----------------------------------------------------------------------------
-- Response interpretations

-- $streaming-response
--
-- Streaming response is a bit tricky as acquiring and releasing a resource
-- (initiating a connection and then closing it in our case) in context of
-- @conduit@ streaming requires working with
-- 'Control.Monad.Trans.Resource.ResourceT' monad transformer. This does not
-- play well with the framework @req@ builds.
--
-- Essentially there are only two ways to make it work:
--
--     * Require that every 'MonadHttp' must be an instance of
--       'MonadResource'. This obviously makes the @req@ package harder to
--       work with and less user-friendly. Not to mention that most of the
--       time the instance won't be necessary.
--     * Use the 'withReqManager' in combination with 'ReturnRequest'
--       response interpretation to get both 'L.Manager' and 'L.Request' and
--       then delegate the work to to a custom callback.
--
-- We go with the second option. Here is an example of how to stream 100000
-- bytes and save them to a file:
--
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import Control.Exception (throwIO)
-- > import Control.Monad.IO.Class (MonadIO (..))
-- > import Control.Monad.Trans.Resource (ResourceT)
-- > import Data.Conduit ((=$=), runConduitRes, ConduitM)
-- > import Network.HTTP.Req
-- > import Network.HTTP.Req.Conduit
-- > import qualified Data.Conduit.Binary as CB
-- >
-- > instance MonadHttp (ConduitM i o (ResourceT IO)) where
-- >   handleHttpException = liftIO . throwIO
-- >
-- > main :: IO ()
-- > main = runConduitRes $ do
-- >   let size = 100000 :: Int
-- >   req' GET (https "httpbin.org" /: "bytes" /~ size) NoReqBody httpSource mempty
-- >     =$= CB.sinkFile "my-favorite-file.bin"

-- | Mostly like 'req' with respect to its arguments, but instead of a hint
-- how to interpret response it takes a callback that allows to perform a
-- request using arbitrary code.

req'
  :: ( MonadHttp  m
     , HttpMethod method
     , HttpBody   body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url' — location of resource
  -> body              -- ^ Body of the request
  -> (L.Request -> L.Manager -> m a) -- ^ How to perform actual request
  -> Option scheme     -- ^ Collection of optional parameters
  -> m a               -- ^ Result
req' method url body m options = do
  request <- responseRequest `liftM` req method url body returnRequest options
  withReqManager (m request)

-- | Perform an HTTP request and get the response as a 'C.Producer'.

httpSource
  :: MonadResource m
  => L.Request         -- ^ Pre-formed 'L.Request'
  -> L.Manager         -- ^ Manger to use
  -> C.Producer m ByteString -- ^ Response body as a 'C.Producer'
httpSource request manager =
  C.bracketP (L.responseOpen request manager) L.responseClose
    (bodyReaderSource . L.responseBody)

----------------------------------------------------------------------------
-- Helpers

-- | This is taken from "Network.HTTP.Client.Conduit" without modifications.

srcToPopperIO :: Source IO ByteString -> L.GivesPopper ()
srcToPopperIO src f = do
  (rsrc0, ()) <- src $$+ return ()
  irsrc <- newIORef rsrc0
  let popper :: IO ByteString
      popper = do
        rsrc <- readIORef irsrc
        (rsrc', mres) <- rsrc $$++ await
        writeIORef irsrc rsrc'
        case mres of
          Nothing -> return B.empty
          Just bs
              | B.null bs -> popper
              | otherwise -> return bs
  f popper

-- | This is taken from "Network.HTTP.Client.Conduit" without modifications.

bodyReaderSource :: MonadIO m => L.BodyReader -> C.Producer m ByteString
bodyReaderSource br = go
  where
    go = do
      bs <- liftIO (L.brRead br)
      unless (B.null bs) $ do
        yield bs
        go
