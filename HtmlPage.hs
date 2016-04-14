{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances    #-}
{-#LANGUAGE OverlappingInstances #-}

module HtmlPage
  ( -- * Types
    HtmlPage, Tag(..), TagId, Attributes,
    -- * Content functions
    emptyContent, emptyPage, (?), (?=), (<<+), (<+), (<<),

    -- * Attributes functions
    emptyAttributes, attributeList, (@+), (@?),

    -- * Cursor functions
    tagId, select, nextTagId, parentTagId,

    -- * IO functions
    fromFile,
  ) where

import Data.List (find)
import Data.Text (unpack, strip, pack, toLower)


------------------------------------------------------------------------------
--                                  Types                                   --
------------------------------------------------------------------------------

-- | An HTML page.
type HtmlPage = Content

-- | An HTML tag.
data Tag =
  PairedTag {
    tagName    :: String,
    attributes :: Attributes,
    content    :: Content
  } |
  UnpairedTag {
    tagName    :: String,
    attributes :: Attributes
  } |
  Doctype {
    dtdInformation :: String
  } |
  Comment {
    commentContent :: String
  }

-- | An attribute of a tag.
data Attribute = Attribute {
  attributeName  :: String,
  attributeValue :: String
}

-- | A content (a list containing strings and tags).
type Content = [Either String Tag]

-- | A list of attributes.
type Attributes = [Attribute]

-- | The id of a tag in the page (i.e. a cursor).
type TagId = [Int]


------------------------------------------------------------------------------
--                            Library functions                             --
------------------------------------------------------------------------------

instance Read Content where
  readsPrec _ string
    | null string = [(emptyContent, "")]
    | otherwise   = [getContent string emptyContent]
    where
      -- | Returns the content of a tag from a given string.
      getContent :: String -> Content -> (Content, String)
      getContent "" content = (content, "")
      getContent ('<' : '/' : after) content =
        (content, snd (splitFirst after '>'))
      getContent ('<' : after) content
        | isScript && scriptEmpty = getContent afterScript (content <<+ tag)
        | isScript && not scriptEmpty =
          getContent afterScript (content <<+ (tag << (emptyContent <+ script)))
        | otherwise = case tag of
          PairedTag {} -> getContent afterTag $ content <<+ (tag << tagContent)
          _            -> getContent rest (content <<+ tag)
          where
            isScript = after `startsWith` "script"
            scriptEmpty = null (trim script)
            tag = read tagString :: Tag
            (tagString, rest) = getTagString 0 "" ('<' : after)
            (tagContent, afterTag) = getContent rest emptyContent
            (script, afterScript) = getScript 0 rest
      getContent string content
        | contentEmpty && null after = getContent "" content
        | contentEmpty && (not . null) after = getContent ('<' : after) content
        | otherwise = getContent ('<' : after) (content <+ contentString)
        where
          (contentString, after) = splitFirst string '<'
          contentEmpty = null (trim contentString)

instance Read Tag where
  readsPrec _ ""     = noParse
  readsPrec _ string = [getTag (trim string)]
    where
      -- | Returns a tag from a given string.
      getTag :: String -> (Tag, String)
      getTag ('<' : string)
        | last tagContent == '/' =
          (UnpairedTag tagName attributesUnpairedTag, "")
        | string `startsWith` "!--" = (Comment contentComment, "")
        | string `startsWith` "!doctype" = (Doctype dtdInformation, "")
        | otherwise = (PairedTag tagName attributesPairedTag emptyContent, "")
        where
          tagContent = take (length string - 1) string
          (tagName, attributes) = if ' ' `elem` tagContent
            then splitFirst tagContent ' '
            else splitFirst tagContent '/'
          attributesPairedTag = read attributes :: Attributes
          attributesUnpairedTag =
            read (take (length attributes - 1) attributes) :: Attributes
          contentComment = drop 3 (take (length string - 3) string)
          (_, dtdInformation) = splitFirst_ (dropSpaces attributes) " PUBLIC "
      getTag _ = noParse

instance Read Attribute where
  readsPrec _ string
    | '=' `elem` string = [(Attribute name value, "")]
    | otherwise         = noParse
    where
      (name, valueInQuotes) = splitFirst (trim string) '='
      value = case split valueInQuotes '\"' of
        []     -> []
        value' -> head value'

instance Read Attributes where
  readsPrec _ string = [(getAttributes emptyAttributes "" string, "")]
    where
      -- | Returns a list of attributes from a given string.
      getAttributes :: Attributes -> String -> String -> Attributes
      getAttributes attributes attribute "" = attributes
      getAttributes attributes attribute ('\"' : string) =
        getAttributes (attributes @+ attribute') "" (dropSpaces rest)
        where
          (value, rest) = splitFirst string '\"'
          attribute' = read (attribute ++ "\"" ++ value ++ "\"") :: Attribute
      getAttributes attributes attribute ('\'' : string) =
        getAttributes (attributes @+ attribute') "" (dropSpaces rest)
        where
          (value, rest) = splitFirst string '\''
          attribute' = read (attribute ++ "\'" ++ value ++ "\'") :: Attribute
      getAttributes attributes attribute (char : string) =
        getAttributes attributes (attribute ++ [char]) string

instance Show Attribute where
  show (Attribute name value) = name ++ "=\"" ++ value ++ "\""

instance Show Attributes where
  show attributes = unwords [show attribute | attribute <- attributes]

instance Show Tag where
  show (PairedTag name [] content) =
    "<" ++ name ++ ">" ++ show content ++ "</" ++ name ++ ">"
  show (PairedTag name attributes content) =
    "<" ++ name ++ " " ++ show attributes ++ ">"
      ++ show content ++ "</" ++ name ++ ">"
  show (UnpairedTag name []) = "<" ++ name ++ " />"
  show (UnpairedTag name attributes) =
    "<" ++ name ++ " " ++ show attributes ++ " />"
  show (Doctype []) = "<!DOCTYPE html>"
  show (Doctype dtdInformation) =
    "<!DOCTYPE html PUBLIC " ++ dtdInformation ++ ">"
  show (Comment content) = "<!--" ++ content ++ "-->"

instance Show Content where
  show []                      = ""
  show (Left string : content) = string ++ show (content :: Content)
  show (Right tag   : content) = show tag ++ show (content :: Content)

instance Show TagId where
  show []                = ""
  show [index]           = show index
  show (index : indices) = show index ++ "." ++ show (indices :: TagId)

-- | Returns an empty content.
emptyContent :: Content
emptyContent = []

-- | Returns an empty HTML page.
emptyPage :: HtmlPage
emptyPage = emptyContent

-- | Returns the tag/string at the given position (TagId) in a content.
(?) :: Content -> TagId -> Either String Tag
(?) _ [] = tagNotInPage
(?) content [index]
  | index < length content = content !! index
  | otherwise = tagNotInPage
(?) content (index : indices) = case content !! index of
  Right tag @ (PairedTag _ _ content') -> content' ? indices
  _                                    -> tagNotInPage

-- | Replaces the tag/string at the given position (TagId) in a content.
(?=) :: Content -> (TagId, Either String Tag) -> Content
(?=) _ ([], _) = impossibleToAddTag
(?=) content ([index], stringOrTag)
  | index < length content = content !!= (index, stringOrTag)
  | index == length content = case stringOrTag of
    Left string -> content <+ string
    Right tag   -> content <<+ tag
  | otherwise = impossibleToAddTag
(?=) content (index : indices, stringOrTag)
  | index < length content = case content !! index of
    Right tag @ (PairedTag _ _ content') ->
      content !!= (index, Right $ tag << (content' ?= (indices, stringOrTag)))
    _ -> impossibleToAddTag
  | otherwise = impossibleToAddTag

-- | Adds a tag to a content.
(<<+) :: Content -> Tag -> Content
(<<+) content tag = content ++ [Right tag]

-- | Adds a string to a content.
(<+) :: Content -> String -> Content
(<+) _       ""     = stringMustNotBeEmpty
(<+) content string = content ++ [Left string]

-- | Replaces the content of the given tag by the given content.
(<<) :: Tag -> Content -> Tag
(<<) (PairedTag name attributes _) = PairedTag name attributes
(<<) (UnpairedTag _ _) = unpairedTagsDoNotHaveAContent
(<<) (Doctype _) = doctypesDoNotHaveAContent
(<<) (Comment _) = commentsDoNotHaveAContent

-- | Returns a TagId from a given string.
tagId :: String -> TagId
tagId string = [read index :: Int | index <- split string '.']

-- | Returns a tag matching the given selector string from the given page.
-- A selector is composed of a tag name and is optionally followed by a hash
-- and an HTML id name.
select :: HtmlPage -> String -> (TagId, Tag)
select page selector = case find (tagMatches (null after)) (tagList page) of
  Just tagIdAndTag -> tagIdAndTag
  _                -> noSuchTag
  where
    (tagString, after) = splitFirst selector '#'
    tagMatches _     (_, Comment _) = False
    tagMatches _     (_, Doctype _) = False
    tagMatches True  (_, tag)       = tagString == tagName tag
    tagMatches False (_, tag)       =
      tagString == tagName tag && (attributes tag @? "id" == after)

-- | Returns the next TagId from a given TagId.
nextTagId :: TagId -> Bool -> TagId
nextTagId tagId True  = tagId ++ [0]
nextTagId tagId False = take (length tagId - 1) tagId ++ [last tagId + 1]

-- | Returns the parent TagId of the given TagId.
parentTagId :: TagId -> TagId
parentTagId tagId = take (length tagId - 1) tagId

-- | Returns an empty attribute list.
emptyAttributes :: Attributes
emptyAttributes = []

-- | Returns a list of attributes from a given list of attribute names and
-- values.
attributeList :: [(String, String)] -> Attributes
attributeList namesAndValues = attributes' namesAndValues emptyAttributes
  where
    attributes' [] attributes = attributes
    attributes' ((name, value) : namesAndValues') attributes =
      attributes' namesAndValues' (attributes @+ Attribute name value)

-- | Adds an attribute to an attribute list.
(@+) :: Attributes -> Attribute -> Attributes
(@+) attributes attribute = attributes ++ [attribute]

-- | Returns the value of the attribute matching the given name.
(@?) :: Attributes -> String -> String
(@?) attributes name =
  case find (\attribute -> attributeName attribute == name) attributes of
    Just (Attribute _ value) -> value
    _                        -> ""

-- | Reads a file and produces an HtmlPage.
fromFile :: FilePath -> IO HtmlPage
fromFile filePath = do
  string <- readFile filePath
  return (read string :: Content)


------------------------------------------------------------------------------
--                             Helper functions                             --
------------------------------------------------------------------------------

-- | Replaces, in a given list, the value at the given index with a new given
-- value.
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) [] _ = emptyList
(!!=) list (index, value)
  | index > (length list - 1) = indexTooLarge
  | index < 0 = negativeIndex
  | otherwise = take index list ++ [value] ++ drop (index + 1) list

-- | Returns the content of a script tag.
getScript :: Int -> String -> (String, String)
getScript _                    ""     = ("", "")
getScript numberOfQuotesBefore string =
  if isPair (numberOfQuotesInScript + numberOfQuotesBefore)
    then (script, dropSpaces afterScript)
    else (script ++ script', rest')
  where
    (script, rest) = splitFirst_ string "</script"
    (_, afterScript) = splitFirst rest '>'
    numberOfQuotesInScript = numberOfQuotes script
    (script', rest') =
      getScript (numberOfQuotesBefore + numberOfQuotesInScript) rest

-- | Returns the tag at the beginning of the string.
getTagString :: Int -> String -> String -> (String, String)
getTagString _ _ "" = ("", "")
getTagString numberOfQuotesBefore suffix string
  | null suffix && string `startsWith` "<!--" =
    getTagString numberOfQuotesBefore "-->" string
  | null suffix && string `startsWith` "<" =
    getTagString numberOfQuotesBefore ">" string
  | isPair (numberOfQuotesInTag + numberOfQuotesBefore) = (tag ++ suffix, rest)
  | otherwise = (tag ++ tag', rest')
  where
    (tag, rest) = splitFirst_ string suffix
    numberOfQuotesInTag = numberOfQuotes tag
    (tag', rest') =
      getTagString (numberOfQuotesBefore + numberOfQuotesInTag) suffix rest

-- | Drops the spaces at the beginning of a string.
dropSpaces :: String -> String
dropSpaces (' ' : string)  = dropSpaces string
dropSpaces ('\n' : string) = dropSpaces string
dropSpaces ('\r' : string) = dropSpaces string
dropSpaces ('\t' : string) = dropSpaces string
dropSpaces string          = string

-- | Splits a string in two with a given delimiting character.
splitFirst :: String -> Char -> (String, String)
splitFirst string delimiter = (before, drop 1 after)
  where
    (before, after) = break (== delimiter) string

-- | Splits a string in two with a given delimiting string.
splitFirst_ :: String -> String -> (String, String)
splitFirst_ string delimiter = splitFirst' "" string
  where
    delimiterLength = length delimiter
    splitFirst' before after
      | take delimiterLength after == delimiter =
        (before, drop delimiterLength after)
      | null after = (before, "")
      | otherwise = splitFirst' (before ++ take 1 after) (drop 1 after)

-- | Splits a string with a given delimiting character.
split :: String -> Char -> [String]
split string delimiter = split' [] string
  where
    split' before ""    = before
    split' before after = if null before'
      then split' before after'
      else split' (before ++ [before']) after'
      where
        (before', after') = splitFirst after delimiter

-- | Removes the spaces at the beginning and at the end of a string.
trim :: String -> String
trim string = unpack . strip $ pack string

-- | Returns the number of quotes in a string.
numberOfQuotes :: String -> Int
numberOfQuotes string = quotes string False False
  where
    quotes ""             _     _     = 0
    quotes ('\"' : after) False False = 1 + quotes after True  False
    quotes ('\'' : after) False False = 1 + quotes after False True
    quotes ('\"' : after) True  _     = 1 + quotes after False False
    quotes ('\'' : after) True  _     =     quotes after True  False
    quotes ('\"' : after) _     True  =     quotes after False True
    quotes ('\'' : after) _     True  = 1 + quotes after False False
    quotes (char : after) inDoubleQuotes inSingleQuotes =
      quotes after inDoubleQuotes inSingleQuotes

-- | Indicates whether the given integer is pair.
isPair :: Int -> Bool
isPair int = int `mod` 2 == 0

-- | Indicates whether the first given string starts with the second given
-- string (not case sensitive).
startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith firstString secondString = firstText == secondText
  where
    firstString' = take (length secondString) firstString
    firstText = toLower $ pack firstString'
    secondText = toLower $ pack secondString

-- | Returns a list containing all the tags (and all their associated TagId) of
-- a given Content.
tagList :: Content -> [(TagId, Tag)]
tagList content' = tagList' content' [0]
  where
    tagList' [] _ = []
    tagList' (Left _ : rest) currentTagId = restOfList rest currentTagId
    tagList' (Right tag : rest) currentTagId = case tag of
      PairedTag {} ->
        (currentTagId, tag) : contentList ++ restOfList rest currentTagId
      _            -> (currentTagId, tag) : restOfList rest currentTagId
      where
        contentList = tagList' (content tag) (nextTagId currentTagId True)
    restOfList content tagId = tagList' content (nextTagId tagId False)


------------------------------------------------------------------------------
--                                  Errors                                  --
------------------------------------------------------------------------------

noParse :: a
noParse = error "HtmlPage.read: no parse"

tagNotInPage :: a
tagNotInPage = error "HtmlPage.?: this tag does not exist in the page"

impossibleToAddTag :: a
impossibleToAddTag = error "HtmlPage.?=: impossible to add the tag"

noSuchTag :: a
noSuchTag = error "HtmlPage.tagId: there is no such tag in the given page"

emptyList :: a
emptyList = error "HtmlPage.!!=: empty list"

indexTooLarge :: a
indexTooLarge = error "HtmlPage.!!=: index too large"

negativeIndex :: a
negativeIndex = error "HtmlPage.!!=: negative index"

unpairedTagsDoNotHaveAContent :: a
unpairedTagsDoNotHaveAContent =
  error "HtmlPage.<<: unpaired tags do not have a content"

doctypesDoNotHaveAContent :: a
doctypesDoNotHaveAContent = error "HtmlPage.<<: doctypes do not have a content"

commentsDoNotHaveAContent :: a
commentsDoNotHaveAContent = error "HtmlPage.<<: comments do not have a content"

stringMustNotBeEmpty :: a
stringMustNotBeEmpty = error "HtmlPage.<+: the string must not be empty"
