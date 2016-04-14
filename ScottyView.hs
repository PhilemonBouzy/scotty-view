module ScottyView (getView, getViewFromFile) where

import Web.Scotty (ActionM, liftAndCatchIO, html)
import Data.Text.Lazy (pack)

import HtmlPage (emptyPage, fromFile)
import HtmlPageBuilder (HtmlPageBuilder, run)


------------------------------------------------------------------------------
--                            Library functions                             --
------------------------------------------------------------------------------

-- | Returns a view (ActionM) from a given HtmlPageBuilder.
getView :: HtmlPageBuilder IO () -> ActionM ()
getView builder = do
  page <- liftAndCatchIO $ run builder emptyPage
  html $ pack (show page)

-- | Returns a view (ActionM) from a given file path and HtmlPageBuilder.
getViewFromFile :: String -> HtmlPageBuilder IO () -> ActionM ()
getViewFromFile filePath builder = do
  page <- liftAndCatchIO $ fromFile filePath
  newPage <- liftAndCatchIO $ run builder page
  html $ pack (show newPage)
