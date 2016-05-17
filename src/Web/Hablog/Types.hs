 -- | Types to define Hablog over ScottyT

{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Types where

import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ScottyT, ActionT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)

import Web.Hablog.Config (Config)

type Hablog = ScottyT Text (ReaderT Config IO)
type HablogAction = ActionT Text (ReaderT Config IO)

getCfg :: HablogAction Config
getCfg = lift ask
