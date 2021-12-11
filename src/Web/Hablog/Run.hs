-- | Running Hablog

{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Run where

import Web.Scotty.Trans
import Control.Monad.Reader (runReaderT)
import qualified Data.Text.Lazy as TL
import Network.URI (parseURI)

import Web.Hablog.Router
import Web.Hablog.Config

-- | Run Hablog on HTTP
run :: Config -> IO ()
run cfg =
  scottyT (blogPort cfg) (`runReaderT` cfg) (router $! domain)
  where
    domain = parseURI (TL.unpack $ blogDomain cfg)
