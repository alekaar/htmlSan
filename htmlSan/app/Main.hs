module Main where

import Lib
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.List
import Data.String.Utils

--main :: IO ()
--main = putStr


main :: IO ()
main  = do
    contents <- readFile file
    putStrLn $ show $ renderSanitizedTree contents


file :: String
file = "xss2.txt"

--run escapehtml
runEscapeHTML :: String -> String
runEscapeHTML html = escapehtml $ replace "\"" "\'" $ map toLower html

--turns tags into harmless signs
escapehtml :: String -> String
escapehtml [] = []
escapehtml (c:html) = case c of
  '<'  -> "&lt;" ++ (escapehtml html)
  '>'  -> "&gt;" ++ (escapehtml html)
  '/'  -> "&#x2F;" ++ (escapehtml html)
  '\"' ->"&quot;" ++ (escapehtml html)
  '\'' -> "&#x27;" ++ (escapehtml html)
  _    -> c:(escapehtml html)

--show tag strings structure
runTags :: String -> [Tag String]
runTags html = parseTags html

renderSanitizedTree :: String -> String
renderSanitizedTree html = renderTree $ runSanitizeHTML html

--run sanitizer on tagTree structure
runSanitizeHTML :: String -> [TagTree String]
runSanitizeHTML html =  sanitizeTree
                         $ parseTree
                         $ replace "&gt;" ">"
                         $ replace "&lt;" "<"
                         $ replace "\"" "\'"
                         $ strip
                         $ map toLower
                         $ removeTagWhiteSpace html

removeTagWhiteSpace :: String -> String
removeTagWhiteSpace [] = []
removeTagWhiteSpace (t:' ':html) = case elem t leadingTags of
  True -> t:(removeTagWhiteSpace html)
  False -> t:' ':(removeTagWhiteSpace html)
removeTagWhiteSpace (c:html) = c:(removeTagWhiteSpace html)


leadingTags :: [Char]
leadingTags = ['<', '>', '/']
--sanitize a html TagTree
--TODO gives error when running contents of xss2.txt. Check with main function
--TODO can't sanitize <IMG """><SCRIPT>alert("XSS")</SCRIPT>">, malformed xss
--TODO sanitize other how other frameworks use script tags
--TODO sanitize for more object oriented approach ex "var img = new Image()"
--TODO strip whitespace inside beginning of tag, inside end of tag
--If a disallowed tag is found, drop the tag and move on to the next one.
--If the tag is not disallowed, sanitize the values, the attributes,
--and move on to the next level of tree.
sanitizeTree :: [TagTree String] -> [TagTree String]
sanitizeTree []      = []
sanitizeTree tagtree = case tagtree of
   (TagBranch s a t):htmlTree -> case elem s allowedTags of
     False -> sanitizeTree htmlTree
     True  -> [TagBranch s (sanitizeVals (sanitizeAttr a)) (sanitizeTree t)]
                ++ (sanitizeTree htmlTree)
   (TagLeaf (TagOpen t attrs)):rest -> (TagLeaf (TagOpen t(sanitizeAttr attrs))):(sanitizeTree rest)
   (TagLeaf t):rest                 -> (TagLeaf t):(sanitizeTree rest)

--sanitize attributes to html elements
--if attribute is allowed, check further if it is an URI attribute.
--if it is, check and make sure there is no javascript in the attribute.
--if there is, remove it, else pass it along.
sanitizeAttr :: [(String, String)] -> [(String, String)]
sanitizeAttr []                  = []
sanitizeAttr ((attr, val):attrs) = case elem attr allowedAttributes of
  False     -> sanitizeAttr attrs
  otherwise -> case elem attr uriAttributes of
      True  -> (attr, (checkURI val)):(sanitizeAttr attrs)
      False -> (attr, val):(sanitizeAttr attrs)

--sanitize values that are assigned to attributes
sanitizeVals :: [(String, String)] -> [(String, String)]
sanitizeVals []                  = []
sanitizeVals ((attr, val):attrs) = case checkPrefix val disallowedVals of
  True  -> sanitizeVals attrs
  False -> (attr, val):(sanitizeVals attrs)

--check if some disallowed value is a prefix of the supplied value
checkPrefix :: String -> [String] -> Bool
checkPrefix _ []         = False
checkPrefix val (v:vals) = case isPrefixOf v val of
  False -> checkPrefix val vals
  True  -> True

--Checks that a URI is an actual URI, and not a javascript statement.
{-
Example: src='javascript:alert(1)'
Result:  src=''

Example: src='www.goodsite.com'
Result:  src='www.goodsite.com'
-}
checkURI :: String -> String
checkURI str = case isInfixOf "javascript:" (map toLower str) of
    True  -> []
    False -> str

--list of disallowed tags
{-
script: The most obvious tag to block, can be used to run scripts directly.
img/image: A faulty image source could potentially be dangerous.
iframe: embed other HTML documents in this one, but there is no guarantee that that document is alright.
-}
allowedTags :: [String]
allowedTags = ["div","p","h1","h2","h3"
               ,"h4","h5","h6","ul","ol"
               ,"li","hr","center","br"
               ,"cite","b","table","img"
               ,"tr","th","td","a"]

--allowed attributes to html elements
{-
name: Specifies a name for the element
href: takes an uri that links to an image in an img tag, for example.
src: takes an uri that leads to a resource for the element.
style: contains css styling.
-}
allowedAttributes :: [String]
allowedAttributes = ["name","href","src", "style"]
--attributes that takes/can take a URI as a value

{-
An URI can contain directives such as src='javascript:alert(1)'
-}
uriAttributes :: [String]
uriAttributes = ["href","codebase","cite"
                ,"background","action"
                ,"longdesc","src","profile"
                ,"usemap","classid","data"
                ,"formaction","icon"
                ,"manifest","poster"]

--disallowed prefixes of values that can be assigned to attributes
{-
alert: Makes a popup.
-}
disallowedVals :: [String]
disallowedVals = ["alert"]
