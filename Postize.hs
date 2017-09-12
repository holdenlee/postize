{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XLambdaCase
 -XTemplateHaskell
#-}

module Main where

import Control.Lens

import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String.Utils
--import qualified Data.Text as T
--import Data.Time
import Data.Time.Clock
import Data.Time.Calendar

import System.Directory
import System.Environment

import Text.Printf
 
date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

data Doc =  Doc {_title :: String, _body :: String} deriving (Show)  

makeLenses ''Doc

splitAtCond' :: [a] -> (a -> Bool) -> [a]  -> ([a], [a])
splitAtCond' read f li = case li of
                           [] -> (reverse read, [])
                           h:rest -> if f h 
                                     then (reverse read, li)
                                     else splitAtCond' (h:read) f rest 

splitAtCond :: (a -> Bool) -> [a] -> ([a], [a])
splitAtCond = splitAtCond' []

isHeading :: String -> Bool
isHeading s = 
    case s of
      '#':t:rest -> t/='#'
      _ -> False

isStop :: String -> Bool
isStop s = isHeading s || s == "------ ENTRY ------"    

extractTitle :: String -> String
extractTitle s = 
    case s of
      '#':rest -> extractTitle rest
      ' ':rest -> extractTitle rest
      _ -> s

demoteHeader :: String -> String
demoteHeader s = case s of
                   '#':rest -> rest
                   _ -> s

addDoc :: Doc -> M.Map String Doc -> M.Map String Doc
addDoc d m = if (_title d) `M.member` m
             then M.adjust (body %~ (\x -> x ++ "\n" ++ (_body d))) (_title d) m
             else M.insert (_title d) d m
-- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a

makeDocs' :: M.Map String Doc -> [String] -> M.Map String Doc
makeDocs' m = \case
                [] -> m
                h:rest -> if isHeading h
                          then 
                              let 
                                  (curPost, restOfPosts) = splitAtCond isStop rest
                                  t = trim $ extractTitle h
                                  curPost' = unlines $ map demoteHeader curPost
                              in
                                makeDocs' (addDoc (Doc {_title=t, _body=curPost'}) m) restOfPosts
                          else 
                              let
                                  (_, restOfPosts) = splitAtCond isHeading rest
                              in
                                makeDocs' m restOfPosts

makeDocs :: [String] -> M.Map String Doc
makeDocs = makeDocs' M.empty

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

makeDocHeader :: Doc -> IO String
makeDocHeader doc = 
    do 
      (year, month, day) <- date
      return $ printf "---\ntitle: %s\npublished: %d-%02d-%02d\ntags: none\n---" (_title doc) year month day

makeDocFilename :: Doc -> String
makeDocFilename doc = (makeReplacements (_title doc))++".md"
--map toLower $ 

makeReplacements :: String -> String
makeReplacements = replace "?" "" . replace "!" ""
-- replace " " "-" . 
      
-- either create the doc if it doesn't exist, or open it and write
writeDoc :: FilePath -> Doc -> IO () 
writeDoc dir d = do
  let file = (dir ++ "/" ++ (makeDocFilename d))
  --https://www.rosettacode.org/wiki/Check_that_file_exists#Haskell
  b <- doesFileExist file
  if b
    then appendFile file ("/n/n"++(_body d))
    else 
        do
          header <- makeDocHeader d
          writeFile file (header++"\n\n"++(_body d))

main :: IO [()]
main = do
  args <- getArgs
  let inputF = if (length args >= 1) then args !! 0 else "in.txt"
  let dir = if (length args >= 2) then args !! 1 else "C:/Users/Owner/Dropbox/website/web_private/posts"
  text <- readFile inputF
  let mapOfDocs = makeDocs $ lines text
  mapM (writeDoc dir) $ M.elems mapOfDocs

