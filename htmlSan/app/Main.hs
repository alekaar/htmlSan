module Main where

import Lib
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.List
import Data.String.Utils
import Text.Regex.Posix

--main :: IO ()
--main = putStr


main :: IO ()
main  = do
  contents <- readFile "insert filename"
  putStrLn contents
---------------------------- SYSTEM ONE --------------------------------------
systemOne :: String -> IO ()
systemOne html = do
    let a = map toLower html
    let b = runSanitizeHTML a
    let c = renderTree b
    putStrLn c
---------------------------- SYSTEM TWO --------------------------------------
systemTwo :: String -> IO ()
systemTwo html = do
    let b = parseTags html
    let c = sanitizeEscapeHTML b
    putStrLn c
------------------------------------------------------------------------------

------------------------------------ system one ------------------------------
--given input 1, runs all owasp tests, shows result last to first
runAllTests :: Int -> IO ()
runAllTests 46 = putStrLn "done"
runAllTests filenr = do
  contents <- readFile ("htmlSan/app/test" ++ (show filenr) ++ ".txt")
  runAllTests (filenr + 1)
  --putStrLn $ systemOne contents

--turns tags into harmless signs
escapeSimpleHTML :: String -> String
escapeSimpleHTML [] = []
escapeSimpleHTML (c:html) = case c of
  '<'  -> "&lt;"   ++ (escapeSimpleHTML html)
  '>'  -> "&gt;"   ++ (escapeSimpleHTML html)
  '\"' -> "&quot;" ++ (escapeSimpleHTML html)
  '\'' -> "&#x27;" ++ (escapeSimpleHTML html)
  _    -> c:(escapeSimpleHTML html)

--run sanitizer on tagTree structure
runSanitizeHTML :: String -> [TagTree String]
runSanitizeHTML html =  sanitizeTree
                         $ parseTree
                         $ replace "&gt;" ">"
                         $ replace "&lt;" "<"
                         $ replace "\"" "\'"
                         $ strip
                         $ map toLower html

--sanitize a html TagTree
sanitizeTree :: [TagTree String] -> [TagTree String]
sanitizeTree []      = []
sanitizeTree tagtree = case tagtree of
   (TagBranch s a t):htmlTree -> case elem s allowedTags of
     False -> sanitizeTree htmlTree
     True  -> case elem s uriTags of
       False -> [TagBranch s (sanitizeAttr a) (sanitizeTree t)]
                ++ (sanitizeTree htmlTree)
       True  -> case hasURI a of
         True  -> [TagBranch s (sanitizeAttr a) (sanitizeTree t)]
                  ++ (sanitizeTree htmlTree)
         False -> sanitizeTree htmlTree
   (TagLeaf (TagOpen t attrs)):rest -> (TagLeaf (TagOpen t(sanitizeAttr attrs))):(sanitizeTree rest)
   (TagLeaf t):rest                 -> (TagLeaf t):(sanitizeTree rest)

--sanitize attributes to html elements
sanitizeAttr :: [(String, String)] -> [(String, String)]
sanitizeAttr []                  = []
sanitizeAttr ((attr, val):attrs) = case elem attr allowedAttributes of
  False     -> sanitizeAttr attrs
  otherwise -> case elem attr uriAttributes of
      True  -> (attr, (checkURI val)):(sanitizeAttr attrs)
      False -> (attr, val):(sanitizeAttr attrs)

--check if some disallowed value is a prefix of the supplied value
checkPrefix :: String -> [String] -> Bool
checkPrefix _ []         = False
checkPrefix val (v:vals) = case isPrefixOf v val of
  False -> checkPrefix val vals
  True  -> True

--Checks that a URI is an actual URI, and not a javascript statement.
{-
-}
checkURI :: String -> String
checkURI str = case isInfixOf "javascript:" (map toLower str) of
    True  -> []
    False -> case (escapeSimpleHTML str) =~ "((http://|https://){1}[a-z]{3,}\\.{1}[a-z]+(\\.{1}[a-z]*){1,2})" :: (String, String, String) of
      (a,"",c) -> ""
      (a,b,c)  -> case c =~ "(/[a-z]*(/[a-z]*)*.{1}(php|html|js){1}(\\?([a-z]*=[1234567890a-z]*)(&[a-z]*=[1234567890a-z]*)*)?)?" :: (String, String, String) of
        (d,e,f)  -> b ++ e

hasURI :: [(String,String)] -> Bool
hasURI []         = False
hasURI ((a,b):xs) = case elem a uriAttributes of
    True  -> True
    False -> hasURI xs

--list of allowed tags
allowedTags :: [String]
allowedTags = ["div","p","h1","h2","h3"
               ,"h4","h5","h6","ul","ol"
               ,"li","hr","center","br"
               ,"cite","b","table","img"
               ,"tr","th","td","a"]

uriTags :: [String]
uriTags = ["img","a"]

--allowed attributes to html elements
allowedAttributes :: [String]
allowedAttributes = ["name","href","src", "style"]

--attributes that takes/can take a URI as a value
uriAttributes :: [String]
uriAttributes = ["href","codebase","cite"
                ,"background","action"
                ,"longdesc","src","profile"
                ,"usemap","classid","data"
                ,"formaction","icon"
                ,"manifest","poster"]

--------------------------------------------System two-----------------------------------------
sanitizeEscapeHTML :: [Tag String] -> String
sanitizeEscapeHTML []     = []
sanitizeEscapeHTML (x:xs) = case x of
  (TagText t)    -> (sanitizeText t) ++ (sanitizeEscapeHTML xs)
  (TagOpen t at) -> case elem t allowedTags of
    True  -> "<"    ++ t ++ (sanitizeEscapeAttr at) ++ ">"    ++ (sanitizeEscapeHTML xs)
    False -> escapeAllHTML ("&lt;" ++ t ++ (sanitizeEscapeAttr at) ++ "&gt;") ++ (sanitizeEscapeHTML xs)
  (TagClose t)   -> case elem t allowedTags of
    True  -> "</"        ++ t ++ ">"    ++ (sanitizeEscapeHTML xs)
    False -> escapeAllHTML ("&lt;&#x2F" ++ t ++ "&gt;") ++ (sanitizeEscapeHTML xs)
  (TagComment t) -> "<!--" ++ t ++ "-->"

sanitizeEscapeAttr :: [(String, String)] -> String
sanitizeEscapeAttr [] = []
sanitizeEscapeAttr ((x,y):xs) = case elem x allowedAttributes of
    True  -> case elem x uriAttributes of
      True  -> " " ++ x ++ "=" ++ "\'" ++ (checkURI y) ++ "\'" ++ (sanitizeEscapeAttr xs)
      False -> " " ++ x ++ "=" ++ "\'" ++ y ++ "\'" ++ (sanitizeEscapeAttr xs)
    False -> escapeAllHTML (" " ++ x ++ "&#61" ++ "\'" ++ y ++ "\'") ++ (sanitizeEscapeAttr xs)

sanitizeText :: String -> String
sanitizeText text = case parseTags text of
  ((TagText res):[]) -> text
  tags                 -> sanitizeEscapeHTML tags

escapeAllHTML :: String -> String
escapeAllHTML []       = []
escapeAllHTML (c:html) = case c of
  '<'  -> "&lt;"   ++ (escapeAllHTML html)
  '>'  -> "&gt;"   ++ (escapeAllHTML html)
  '\"' -> "&quot;" ++ (escapeAllHTML html)
  '\'' -> "&#x27;" ++ (escapeAllHTML html)
  '/'  -> "&#x2F"  ++ (escapeAllHTML html)
  ' '  -> "&#32;"  ++ (escapeAllHTML html)
  '`'  -> "&#96;"  ++ (escapeAllHTML html)
  '!'  -> "&#33;"  ++ (escapeAllHTML html)
  '@'  -> "&#64;"  ++ (escapeAllHTML html)
  '$'  -> "&#36;"  ++ (escapeAllHTML html)
  '%'  -> "&#37;"  ++ (escapeAllHTML html)
  '('  -> "&#40;"  ++ (escapeAllHTML html)
  ')'  -> "&#41;"  ++ (escapeAllHTML html)
  '='  -> "&#61;"  ++ (escapeAllHTML html)
  '+'  -> "&#43;"  ++ (escapeAllHTML html)
  '{'  -> "&#123;" ++ (escapeAllHTML html)
  '}'  -> "&#125;" ++ (escapeAllHTML html)
  '['  -> "&#91;"  ++ (escapeAllHTML html)
  ']'  -> "&#93;"  ++ (escapeAllHTML html)
  _    -> c:(escapeAllHTML html)
