module Main where

import Web.Scotty (ActionM, scotty, get, capture)

import qualified HtmlPageBuilder as HTML
import ScottyView

-- | The main.
-- The HTML file is a slightly different version of the "Real Estate or Travel"
-- template from foundation.zurb.com.
main :: IO ()
main = scotty 3000 $ get (capture "/") (readHtmlFileView "index.html" info)

-- | A simple to-do list.
toDoListView :: [String] -> String -> ActionM ()
toDoListView list info = getView $ do
  HTML.doctype "html5"
  HTML.title "To-do list" $ HTML.attributeList []
  HTML.meta $ HTML.attributeList [("charset", "UTF-8")]
  HTML.div $ HTML.attributeList [("id", "to-do")]
  HTML.h1 "To-do list" $ HTML.attributeList []
  HTML.unorderedList list (HTML.attributeList [("class", "to-do-list")])
    (HTML.attributeList [("class", "to-do-list-element")])
  HTML.parent
  HTML.div $ HTML.attributeList [("id", "info")]
  HTML.p $ HTML.attributeList []
  HTML.text info

-- | Reads a file, changes a paragraph in it and returns the view (ActionM).
readHtmlFileView :: FilePath -> String -> ActionM ()
readHtmlFileView filePath info = getViewFromFile filePath $ do
  HTML.goIn "p#info"
  HTML.text (take 60 info ++ "…")

list :: [String]
list = ["Repair the roof", "Repaint the bathroom", "Call Dominique"]

info :: String
info = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque ex "
  ++ "lacus, consequat non mi sed, tempor malesuada risus. Donec nec consequat "
  ++ "massa. In eros neque, efficitur a arcu quis, imperdiet tristique leo. In "
  ++ "quis augue ultrices, condimentum est fringilla, auctor leo. Morbi non "
  ++ "felis nulla. Proin tempus interdum leo, ac feugiat purus tempor nec."
