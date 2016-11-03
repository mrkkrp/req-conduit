-- |
-- Module      :  Network.HTTP.Req.Conduit
-- Copyright   :  © 2016 Mark Karpov, Michael Snoyman
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

module Network.HTTP.Req.Conduit
  ( -- * Request bodies
    ReqBodySource (..)
    -- * Response interpretations
 )
where

import Data.ByteString (ByteString)
import Data.Conduit (Source, ($$+), ($$++), await)
import Data.IORef
import Data.Int (Int64)
import Network.HTTP.Req
import qualified Data.ByteString              as B
import qualified Data.Conduit                 as C
import qualified Network.HTTP.Client          as L

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

-- this section can only be outlined when API for response interpretation is
-- complete

-- TODO Perform HTTP request and get the response as a source. Also we want
-- to provide a wrapper that saves body in a file.

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
