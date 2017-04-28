module Main where

import Lib
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

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
     False -> [TagBranch s (sanitizeAttr a) (sanitizeTree t)] ++ (sanitizeTree htmlTree)

--sanitize attributes to html elements
sanitizeAttr :: [(String, String)] -> [(String, String)]
sanitizeAttr []                = []
sanitizeAttr ((attr, val):attrs) = case elem attr disallowedAttr of
  True -> sanitizeAttr attrs
  _    -> (attr, val):(sanitizeAttr attrs)


--list of disallowed tags
disallowedTags :: [String]
disallowedTags = ["script", "img", "image"]

--disallowed attributes to html elements
disallowedAttr :: [String]
disallowedAttr = ["onerror"]
