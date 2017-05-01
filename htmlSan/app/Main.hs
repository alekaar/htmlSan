module Main where

import Lib
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.List
import Data.String.Utils

main :: IO ()
main = someFunc

--show tag strings structure
runTags :: String -> [Tag String]
runTags html = parseTags html

--run sanitizer on tagTree structure
runSanitizeHTML :: String -> [TagTree String]
runSanitizeHTML html = sanitizeTree $ parseTree
                         $ replace "&gt;" ">" $ replace "&lt;" "<"
                            $ strip $ map toLower html

--sanitize a html TagTree
--TODO sanitize other how other frameworks use script tags
--TODO sanitize for more object oriented approach ex "var img = new Image()"
--TODO strip whitespace inside beginning of tag, inside end of tag
--If a disallowed tag is found, drop the tag and move on to the next one.
--If the tag is not disallowed, sanitize the values, the attributes,
--and move on to the next level of tree.
sanitizeTree :: [TagTree String] -> [TagTree String]
sanitizeTree []      = []
sanitizeTree tagtree = case tagtree of
   [TagLeaf (TagText s)]                  -> [TagLeaf (TagText s)]
   (tag@(TagBranch s a t):htmlTree) -> case elem s disallowedTags of
     True  -> (sanitizeTag tag):(sanitizeTree htmlTree)
     False -> [TagBranch s (sanitizeVals (sanitizeAttr a)) (sanitizeTree t)] ++ (sanitizeTree htmlTree)

--Sanitizes a HTML tag. Just dropping the tag is easier, but just dropping enough
--to disable the execution of javascript is even better.
--about iframe, would rather exclude it all together, but code gets ugly if it
--matches for it in a previous stage.
sanitizeTag :: TagTree String -> TagTree String
sanitizeTag inp@(TagBranch tag attrlist leaf) = case tag of
    ("script") -> TagBranch "p" (sanitizeAttr attrlist) leaf
    ("img")    -> TagBranch "img" (checkSrc attrlist) leaf
    ("image")  -> TagBranch "image" (checkSrc attrlist) leaf
    ("iframe") -> TagBranch "p" [] [TagLeaf (TagText "iframe not supported")]
    otherwise  -> inp

--sanitize attributes to html elements
--If disallowed attribute is encountered, drop the attribute all together.
sanitizeAttr :: [(String, String)] -> [(String, String)]
sanitizeAttr []                  = []
sanitizeAttr ((attr, val):attrs) = case elem attr disallowedAttr of
  True -> sanitizeAttr attrs
  _    -> (attr, val):(sanitizeAttr attrs)

--sanitize values that are assigned to attributes
--If disallowed value is encoutered, the value is dropped all together.
sanitizeVals :: [(String, String)] -> [(String, String)]
sanitizeVals []                  = []
sanitizeVals ((attr, val):attrs) = case checkPrefix val disallowedVals of
  True -> sanitizeVals attrs
  _    -> (attr, val):(sanitizeVals attrs)

--checks that an image src is from an allowed origin
checkSrc :: [(String, String)] -> [(String, String)]
checkSrc ((a,b):rest) = case a of
    ("src")   -> case isPrefixOf "http://" b of
      True  -> case checkPrefix b allowedImgOrigin of
        True  -> (a,b):rest
        False -> rest
      False -> case checkPrefix ("http://" ++ b) allowedImgOrigin of
        True  -> (a,b):rest
        False -> rest
    otherwise -> (a,b):(checkSrc rest)

--check if some disallowed value is a prefix of the supplied value
checkPrefix :: String -> [String] -> Bool
checkPrefix _ []         = False
checkPrefix val (v:vals) = case isPrefixOf v val of
  False -> checkPrefix val vals
  _     -> True

--list of disallowed tags
{-
script: The most obvious tag to block, can be used to run scripts directly.
img/image: A faulty image source could potentially be dangerous.
iframe: embed other HTML documents in this one, but there is no guarantee that that document is alright.
-}
disallowedTags :: [String]
disallowedTags = ["script", "img", "image", "iframe"]

--disallowed attributes to html elements
{-
onerror: Can be used in combination with a faulty img to run scripts.
onmouseover: Can run scripts if clients mouse hover over element.
-}
disallowedAttr :: [String]
disallowedAttr = windowObjectEvents ++ formEvents ++ keyboardEvents ++ mouseEvents ++ dragEvents ++ clipboardEvents ++ mediaEvents ++ miscEvents

windowObjectEvents:: [String]
windowObjectEvents = ["onafterprint","onbeforeprint","onbeforeunload","onerror","onhashchange","onload","onmessage","onoffline","ononline","onpagehide","onpageshow","onpopstate","onresize","onstorage","onunload"]

formEvents :: [String]
formEvents = ["onblur","inchange","oncontextmenu","onfocus","oninput","oninvalid","onreset","onsearch","onselect","onsubmit"]

keyboardEvents :: [String]
keyboardEvents = ["onkeydown","onkeypress","onkeyup"]

mouseEvents :: [String]
mouseEvents = ["onclick","ondbclick","onmousedown","onmousemove","onmouseout","onmouseover","onmouseup","onmousewheel","onwheel"]

dragEvents :: [String]
dragEvents = ["ondrag","ondragend","ondragenter","ondragleave","ondragover","ondrop","onscroll"]

clipboardEvents :: [String]
clipboardEvents = ["oncopy","oncut","onpaste"]

mediaEvents :: [String]
mediaEvents = ["onabort","oncanplay","oncanplaythrough","oncuechange","ondurationchange","onemptied","onended","onerror","onloadeddata","onloadedmetadata","onloadstart","onpause","onplay","onplaying","onprogress","onratechange","onseeked","onseeking","onstalled","onsuspend","ontimeupdate","onvolumechange","onwaiting"]

miscEvents :: [String]
miscEvents = ["onshow","ontoggle"]

--disallowed prefixes of values that can be assigned to attributes
{-
alert: Makes a popup.
-}
disallowedVals :: [String]
disallowedVals = ["alert"]

--when loading image sources, these are the allowed origins of images.
{-

-}
allowedImgOrigin :: [String]
allowedImgOrigin = ["http://goodwebsite.com"]
