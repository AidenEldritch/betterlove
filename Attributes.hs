import Date.Time.Clock
import Data.Time.Calendar

import System.FilePath
import System.IO

-- |The 'ancestry' function gets the "ancestors" of the file, i.e. it auto
-- generates the parent pages of the file.
ancestry :: FilePath -> [String]
ancestry x = splitOn pathSeparator x

-- |The 'date' function gets the current date and time and pretty-prints it.
date :: String
date = do
    now <- getCurrentTime
    takeWhile (/= ' ') now

-- |The 'url' function generates a URL based on the parent directory of the
-- file.
url :: FilePath -> String
url x = (drop 1 takeDirectory x) ++ ".html"
    
-- |The 'tags' function parses any YAML in the head of the Markdown file.
tags :: FilePath -> String
tags x = do
    contents <- readFile x

-- |The 'title' function extracts the title of the post from the file. Accepts
-- one argument of type 'FilePath', aka 'String'.
title :: FilePath -> String
title x = do
    contents <- readFile x
    drop 2 $ head $ filter ("# " isPrefixOf) $ lines contents
