{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page controller.

module HL.Controller.Downloads where

import HL.Controller
import HL.Model.Markdown
import HL.View.Downloads
import HL.View

import System.Environment (lookupEnv)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

-- | Downloads controller.
-- getDownloadsR :: C (Html ())
-- getDownloadsR = lucid downloadsV
getDownloadsR :: C (Html())
getDownloadsR = do
  what <- fmap (fromMaybe "") $ io $ lookupEnv "HL_DOWNLOADS"
  case what of
    ('a':'1':' ':hp_root)  -> lucid (downloadsValt 1 (T.pack hp_root))
    ('a':'2':' ':url)      -> redirect url
    ('b':' ':hp_root)      -> lucid (downloadsValt 3 (T.pack hp_root))
    _                      -> lucid downloadsV

-- | Downloads for particular OS.
getDownloadsForR :: OS -> C (Html ())
getDownloadsForR os =
  do manualInstall <- io (getMarkdown "manual-install.md")
     autoInstall <- io (getMarkdown autoFp)
     lucid (downloadsForV os autoInstall manualInstall)
  where autoFp =
          case os of
            Windows -> "windows-install.md"
            OSX -> "osx-install.md"
            Linux -> "linux-install.md"
