module Main where

import Lib
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.List

main :: IO ()
main = someFunc

--show tag strings structure
runTags :: String -> [Tag String]
runTags html = parseTags html

--show tagTree structure
runTagTree :: String -> [TagTree String]
runTagTree html = sanitizeTree $ parseTree html

--sanitize a html TagTree
--TODO maybe have to sanitize leaf
sanitizeTree :: [TagTree String] -> [TagTree String]
sanitizeTree []      = []
sanitizeTree tagtree = case tagtree of
   [TagLeaf s] -> [TagLeaf s]
   ((TagBranch s a t):htmlTree) -> case elem s disallowedTags of
     True  -> sanitizeTree htmlTree
     False -> [TagBranch s (sanitizeVals (sanitizeAttr a)) (sanitizeTree t)] ++ (sanitizeTree htmlTree)

--sanitize attributes to html elements
sanitizeAttr :: [(String, String)] -> [(String, String)]
sanitizeAttr []                = []
sanitizeAttr ((attr, val):attrs) = case elem attr disallowedAttr of
  True -> sanitizeAttr attrs
  _    -> (attr, val):(sanitizeAttr attrs)

--sanitize values that are assigned to attributes
sanitizeVals :: [(String, String)] -> [(String, String)]
sanitizeVals [] = []
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
disallowedTags :: [String]
disallowedTags = ["script", "img", "image"]

--disallowed attributes to html elements
disallowedAttr :: [String]
disallowedAttr = ["onerror", "onmouseover"]

--disallowed values that can be assigned to attributes
disallowedVals :: [String]
disallowedVals = ["alert"]
