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
sanitizeTree :: [TagTree String] -> [TagTree String]
sanitizeTree []      = []
sanitizeTree tagtree = case tagtree of
   [TagLeaf (TagText s)]                  -> [TagLeaf (TagText s)]
   ((TagBranch s a t):htmlTree) -> case elem s disallowedTags of
     True  -> sanitizeTree htmlTree
     False -> [TagBranch s (sanitizeVals (sanitizeAttr a)) (sanitizeTree t)] ++ (sanitizeTree htmlTree)

--sanitize attributes to html elements
sanitizeAttr :: [(String, String)] -> [(String, String)]
sanitizeAttr []                  = []
sanitizeAttr ((attr, val):attrs) = case elem attr disallowedAttr of
  True -> sanitizeAttr attrs
  _    -> (attr, val):(sanitizeAttr attrs)

--sanitize values that are assigned to attributes
sanitizeVals :: [(String, String)] -> [(String, String)]
sanitizeVals []                  = []
sanitizeVals ((attr, val):attrs) = case checkPrefix val disallowedVals of
  True -> sanitizeVals attrs
  _    -> (attr, val):(sanitizeVals attrs)

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
-}
disallowedTags :: [String]
disallowedTags = ["script", "img", "image"]

--disallowed attributes to html elements
{-
onerror: Can be used in combination with a faulty img to run scripts.
onmouseover: Can run scripts if clients mouse hover over element.
-}
disallowedAttr :: [String]
disallowedAttr = ["onerror", "onmouseover"]

--disallowed prefixes of values that can be assigned to attributes
{-
alert: Makes a popup.
-}
disallowedVals :: [String]
disallowedVals = ["alert"]
