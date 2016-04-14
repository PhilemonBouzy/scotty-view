module HtmlPageBuilder
  ( -- * Types
    HtmlPageBuilder(..),

    -- * Initialize
    doctype,

    -- * Add HTML elements
    meta, link, base, title, h1, h2, h3, h4, h5, h6, div, header, footer,
    section, article, aside, main, nav, p, ul, ol, li, br, hr, comment, text,

    -- * Derived functions
    unorderedList,

    -- * Cursor functions
    parent, goTo, goIn,

    -- * Run function
    run,

    -- * Functions from HtmlPage
    attributeList
  ) where

import Prelude (Functor, Monad, String, Bool(..), Either(..), ($), (.), (++),
  error, return, (>>=), fst, snd, flip)
import Control.Applicative

import HtmlPage


------------------------------------------------------------------------------
--                                  Types                                   --
------------------------------------------------------------------------------

-- | The state of an HtmlPageBuilder. The TagId is the position at which the
-- next element may be added (i.e. a cursor).
type HtmlPageBuilderState = (TagId, HtmlPage)

-- | An HtmlPage builder.
data HtmlPageBuilder m a =
  HtmlPageBuilder (HtmlPageBuilderState -> m (a, HtmlPageBuilderState))


------------------------------------------------------------------------------
--                            Library functions                             --
------------------------------------------------------------------------------

instance Applicative (HtmlPageBuilder m)

instance Functor (HtmlPageBuilder m)

instance Monad m => Monad (HtmlPageBuilder m) where
  -- | Returns a new HtmlPageBuilder from a given result.
  return result = HtmlPageBuilder $ \state -> return (result, state)

  -- | Returns a new HtmlPageBuilder from a given HtmlPageBuilder and
  -- function.
  HtmlPageBuilder builder >>= function = HtmlPageBuilder $ \state1 -> do
    (result, state2) <- builder state1
    let HtmlPageBuilder builder2 = function result in builder2 state2

-- | Creates the skeleton of the page (the doctype as well as the html, head
-- and body tags).
doctype :: (Monad m) => String -> HtmlPageBuilder m ()
doctype version = HtmlPageBuilder $ \state -> case snd state of
  [] -> case version of
    "html5"               -> return ((), (tagId "1.1.0", html5))
    "xhtml1-strict"       -> return ((), (tagId "1.1.0", xhtml1Strict))
    "xhtml1-transitional" -> return ((), (tagId "1.1.0", xhtml1Transitional))
    "xhtml11"             -> return ((), (tagId "1.1.0", xhtml11))
  _  -> pageMustBeEmpty
  where
    headTag = PairedTag "head" emptyAttributes emptyContent
    bodyTag = PairedTag "body" emptyAttributes emptyContent
    htmlTag attributes =
      PairedTag "html" attributes $ emptyContent <<+ headTag <<+ bodyTag
    html5 = emptyPage <<+ doctypeHtml5 <<+ htmlTag emptyAttributes
    xhtmlNameSpace = attributeList [("xmlns", "http://www.w3.org/1999/xhtml")]
    xhtml1Strict = emptyContent <<+ doctypeXhtml1Strict
      <<+ htmlTag xhtmlNameSpace
    xhtml1Transitional = emptyContent <<+ doctypeXhtml1Transitional
      <<+ htmlTag xhtmlNameSpace
    xhtml11 = emptyContent <<+ doctypeXhtml11 <<+ htmlTag xhtmlNameSpace

-- | Adds a meta tag to the head.
meta :: Monad m => Attributes -> HtmlPageBuilder m ()
meta = unpairedTagInHead "meta"

-- | Adds a link tag to the head.
link :: Monad m => Attributes -> HtmlPageBuilder m ()
link = unpairedTagInHead "link"

-- | Adds a base tag to the head.
base :: Monad m => Attributes -> HtmlPageBuilder m ()
base = unpairedTagInHead "base"

-- | Defines the title of the page.
title :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
title = flip $ pairedTagInHead "title"

-- | Adds a level 1 title to the page body.
h1 :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
h1 = simplePairedTagInBody "h1"

-- | Adds a level 2 title to the page body.
h2 :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
h2 = simplePairedTagInBody "h2"

-- | Adds a level 3 title to the page body.
h3 :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
h3 = simplePairedTagInBody "h3"

-- | Adds a level 4 title to the page body.
h4 :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
h4 = simplePairedTagInBody "h4"

-- | Adds a level 5 title to the page body.
h5 :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
h5 = simplePairedTagInBody "h5"

-- | Adds a level 6 title to the page body.
h6 :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
h6 = simplePairedTagInBody "h6"

-- | Adds an empty div tag to the page body and puts the cursor in it.
div :: Monad m => Attributes -> HtmlPageBuilder m ()
div = pairedTagInBody "div"

-- | Adds an empty header tag to the page body and puts the cursor in it.
header :: Monad m => Attributes -> HtmlPageBuilder m ()
header = pairedTagInBody "header"

-- | Adds an empty footer tag to the page body and puts the cursor in it.
footer :: Monad m => Attributes -> HtmlPageBuilder m ()
footer = pairedTagInBody "footer"

-- | Adds an empty section tag to the page body and puts the cursor in it.
section :: Monad m => Attributes -> HtmlPageBuilder m ()
section = pairedTagInBody "section"

-- | Adds an empty article tag to the page body and puts the cursor in it.
article :: Monad m => Attributes -> HtmlPageBuilder m ()
article = pairedTagInBody "article"

-- | Adds an empty aside tag to the page body and puts the cursor in it.
aside :: Monad m => Attributes -> HtmlPageBuilder m ()
aside = pairedTagInBody "aside"

-- | Adds an empty main tag to the page body and puts the cursor in it.
main :: Monad m => Attributes -> HtmlPageBuilder m ()
main = pairedTagInBody "main"

-- | Adds an empty nav tag to the page body and puts the cursor in it.
nav :: Monad m => Attributes -> HtmlPageBuilder m ()
nav = pairedTagInBody "nav"

-- | Adds an empty p tag to the page body and puts the cursor in it.
p :: Monad m => Attributes -> HtmlPageBuilder m ()
p = pairedTagInBody "p"

-- | Adds an empty ul tag to the page body and puts the cursor in it.
ul :: Monad m => Attributes -> HtmlPageBuilder m ()
ul = pairedTagInBody "ul"

-- | Adds an empty ol tag to the page body and puts the cursor in it.
ol :: Monad m => Attributes -> HtmlPageBuilder m ()
ol = pairedTagInBody "ol"

-- | Adds a list element to the page body.
li :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
li = simplePairedTagInBody "li"

-- | Adds a line break to the page body.
br :: Monad m => HtmlPageBuilder m ()
br = unpairedTagInBody "br" $ attributeList []

-- | Adds a thematic break to the page body.
hr :: Monad m => Attributes -> HtmlPageBuilder m ()
hr = unpairedTagInBody "hr"

-- | Adds a comment to the body.
comment :: Monad m => String -> HtmlPageBuilder m ()
comment string = HtmlPageBuilder $ \(tagId, page) ->
  return ((), (nextTagId tagId False, page ?= (tagId, Right (Comment string))))

-- | Inserts some text in a tag.
text :: Monad m => String -> HtmlPageBuilder m ()
text string = HtmlPageBuilder $ \(tagId, page) ->
  return ((), (nextTagId tagId False, page ?= (tagId, Left string)))

-- | Generates an unordered list from a given list of string and adds it to
-- the page body.
unorderedList :: Monad m
              => [String] -> Attributes -> Attributes -> HtmlPageBuilder m ()
unorderedList list ulAttributes liAttributes = do
  ul ulAttributes
  listToLi list
  where
    listToLi []              = parent
    listToLi (string : rest) = do
      li string liAttributes
      listToLi rest

-- | Puts the cursor back to the parent element (after the element in which
-- the cursor was).
parent :: Monad m => HtmlPageBuilder m ()
parent = HtmlPageBuilder $ \(tagId, page) ->
  return ((), (nextTagId (parentTagId tagId) False, page))

-- | Puts the cursor on the tag matching the given selector string.
goTo :: Monad m => String -> HtmlPageBuilder m ()
goTo selector = HtmlPageBuilder $ \(tagId, page) ->
  return ((), (fst (page `select` selector), page))

-- | Puts the cursor in the tag matching the given selector string.
goIn :: Monad m => String -> HtmlPageBuilder m ()
goIn selector = HtmlPageBuilder $ \(tagId, page) ->
  return ((), (nextTagId (fst (page `select` selector)) True, page))

-- | Runs a builder program and returns the generated HtmlPage.
run :: Monad m => HtmlPageBuilder m a -> HtmlPage -> m HtmlPage
run (HtmlPageBuilder program) page = do
  builderState <- program ([0], page)
  return ((snd . snd) builderState)


------------------------------------------------------------------------------
--                             Helper functions                             --
------------------------------------------------------------------------------

-- | Adds a given tag to the head.
tagInHead :: Monad m => Tag -> HtmlPageBuilder m ()
tagInHead tag = HtmlPageBuilder $ \(tagId, page) -> do
  let (headTagId, headTag @ (PairedTag _ _ headContent)) = page `select` "head"
      newHeadTag = headTag << (headContent <<+ tag)
  return ((), (tagId, page ?= (headTagId, Right newHeadTag)))

-- | Adds an unpaired tag to the head.
unpairedTagInHead :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
unpairedTagInHead tagName attributes =
  tagInHead $ UnpairedTag tagName attributes

-- | Adds a paired tag to the head.
pairedTagInHead :: Monad m
                => String -> Attributes -> String -> HtmlPageBuilder m ()
pairedTagInHead tagName attributes content =
  tagInHead $ PairedTag tagName attributes (emptyContent <+ content)

-- | Adds an empty paired tag to the body and puts the cursor in it.
pairedTagInBody :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
pairedTagInBody tagName attributes = HtmlPageBuilder $ \(tagId, page) ->
  return ((), (nextTagId tagId True, page ?= (tagId, Right tag)))
  where
    tag = PairedTag tagName attributes emptyContent

-- | Adds a paired tag with the given content string to the body.
simplePairedTagInBody :: Monad m
                      => String -> String -> Attributes -> HtmlPageBuilder m ()
simplePairedTagInBody tagName content attributes = do
  pairedTagInBody tagName attributes
  HtmlPageBuilder $ \(tagId, page) -> do
    let newTagId = nextTagId (parentTagId tagId) False
    return ((), (newTagId, page ?= (tagId, Left content)))

unpairedTagInBody :: Monad m => String -> Attributes -> HtmlPageBuilder m ()
unpairedTagInBody tagName attributes = HtmlPageBuilder $ \(tagId, page) ->
  return ((), (nextTagId tagId False, page ?= (tagId, Right tag)))
  where
    tag = UnpairedTag tagName attributes


------------------------------------------------------------------------------
--                                 Doctypes                                 --
------------------------------------------------------------------------------

doctypeHtml5 :: Tag
doctypeHtml5 = Doctype ""

doctypeXhtml1Strict :: Tag
doctypeXhtml1Strict = Doctype $ "\"-//W3C//DTD XHTML 1.0 Strict//EN\" "
  ++ "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\""

doctypeXhtml1Transitional :: Tag
doctypeXhtml1Transitional = Doctype $
  "\"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
  ++ "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\""

doctypeXhtml11 :: Tag
doctypeXhtml11 = Doctype $ "\"-//W3C//DTD XHTML 1.1//EN\" "
  ++ "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\""


------------------------------------------------------------------------------
--                                  Errors                                  --
------------------------------------------------------------------------------

pageMustBeEmpty :: a
pageMustBeEmpty = error "HtmlPageBuilder.doctype: the page must be empty"
