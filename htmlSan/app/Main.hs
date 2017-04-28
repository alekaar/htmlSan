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

--sanitize a html tree
sanitizeTree :: [TagTree String] -> [TagTree String]
sanitizeTree htmlTree = htmlTree
