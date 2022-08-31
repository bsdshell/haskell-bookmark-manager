-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
{-# LANGUAGE MultiWayIf #-} 
-- import Turtle
-- echo "turtle"

-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.Char
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Control.Monad (unless, when)
import Control.Concurrent
import Control.Lens hiding (pre)

import qualified Text.Regex.TDFA as TD

--import Data.Array

-- import Graphics.Rendering.OpenGL as GL 
-- import Graphics.Rendering.OpenGL.GLU.Matrix as GM  
-- import qualified Graphics.UI.GLFW as G
-- import Data.Set(Set) 
-- import qualified Data.Set as S 

-- |  end_fold ,}}}

import qualified Data.Text                 as TS
import           Data.Int (Int64)
import           Database.SQLite.Simple hiding (bind)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import AronModule
import AronAlias
  
sw = show

-- zo - open
-- za - close


   
{-|
instance ToRow Image where
  toRow (Image _iid imagename uid) = toRow (imagename, uid)

queryShellHistory::IO[String]
queryShellHistory = do
          home <- getEnv "HOME"
          conn <- open $ dbfile home
          cmdsql <- query_ conn sql_select ::IO [ShellHistory]
          let cmdList = let ls = map (shcmd) cmdsql::[TS.Text] in map toStr ls::[String]
          return cmdList
      where
          sql_select = Query {fromQuery = toSText "SELECT id, shcmd FROM ShellHistory"}
          dbfile home = home </> "myfile/bitbucket/testfile/ShellHistory.db"
          hisFile home = home </> "myfile/bitbucket/shell/dot_bash_history_test"

row <- queryNamed conn "SELECT * FROM user WHERE email = :email AND password = :password" [":password" := password_, ":email" := email_] :: IO [User]
              print row

CREATE TABLE items(
    id INTEGER PRIMARY KEY,
    guid TEXT UNIQUE NOT NULL,
    /* The "parentid" from the record. */
    parentGuid TEXT,
    /* The server modified time, in milliseconds. */
    serverModified INTEGER NOT NULL DEFAULT 0,
    needsMerge BOOLEAN NOT NULL DEFAULT 0,
    validity INTEGER NOT NULL DEFAULT 1,
    isDeleted BOOLEAN NOT NULL DEFAULT 0,
    kind INTEGER NOT NULL DEFAULT -1,
    /* The creation date, in milliseconds. */
    dateAdded INTEGER NOT NULL DEFAULT 0,
    title TEXT,

    urlId INTEGER REFERENCES urls(id) ON DELETE SET NULL,
                                ↑  
                                + → TABLE urls
                                       ↓ 
                                       + → id INTEGER PRIMARY KEY
                                     
    keyword TEXT,
    description TEXT,
    loadInSidebar BOOLEAN,
    smartBookmarkName TEXT,
    feedURL TEXT,
    siteURL TEXT
  );
CREATE INDEX itemURLs ON items(urlId);
CREATE INDEX itemKeywords ON items(keyword)
                    WHERE keyword NOT NULL;
CREATE TABLE urls(
    id INTEGER PRIMARY KEY,
     ↑ 
     + → foreign key in TABLE items

    guid TEXT NOT NULL,
    url TEXT NOT NULL,
    hash INTEGER NOT NULL,
    revHost TEXT NOT NULL
  );
CREATE INDEX urlHashes ON urls(hash);

urlId INTEGER REFERENCES urls(id)
              ON DELETE SET NULL,
=>

NOTE:
    If row in TABLE(urls) is deleted from urls table, then
    uriId will be set to NULL in items table

    If row of urls is deleted then all urlId = NULL should be deleted from TABLE(items)

    DELETE FROM items WHERE urlId IS NULL;

Delete title contains string using LIKE, IN:
    DELETE FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%Vansky%'));
-}
  
data FFurls = FFurls
  { ffId :: Int64
  , ffURL :: TS.Text
  , ffHash :: Int64
  } deriving (Eq,Read,Show)

-- | import Database.SQLite.Simple.FromRow
-- | two fields: shId, shcmd
instance FromRow FFurls where
   fromRow = FFurls <$> field <*> field <*> field

-- | import Database.SQLite.Simple.ToRow
instance ToRow FFurls where
   toRow (FFurls _ffId ffURL ffHash) = toRow (ffURL, ffHash)

-- dateAdded INTEGER NOT NULL DEFAULT 0,
data FFItems = FFItems
  { ttId :: Int64
  , ffTitle :: TS.Text
  , dateAdded ::Int64 -- The creation date, in milliseconds.
  } deriving (Eq,Read,Show)

instance FromRow FFItems where
   fromRow = FFItems <$> field <*> field <*> field

instance ToRow FFItems where
   toRow (FFItems _ttId ffTitle dateAdded) = toRow (ffTitle, dateAdded)

data FFBookMarkAll = FFBookMarkAll
  { ttIdx :: Int64
  , ffURLx :: TS.Text
  , ffTitlex :: TS.Text
  , dateAddedx::Int64
  , ffHashx :: Int64
  } deriving (Eq,Read,Show)

instance FromRow FFBookMarkAll where
   fromRow = FFBookMarkAll <$> field <*> field <*> field <*> field <*> field

instance ToRow FFBookMarkAll where
   toRow (FFBookMarkAll _ttIdx ffURLx ffTitlex dateAddedx ffHashx) = toRow (ffURLx, ffTitlex, dateAddedx, ffHashx)

bmfile h = h </> "myfile/bitbucket/testfile/firefox_bookmark_test.sqlite"
dbfile home = home </> "myfile/bitbucket/testfile/ShellHistory.db"
hisFile home = home </> "myfile/bitbucket/shell/dot_bash_history_test"


mkFFBookMarkAll::FFBookMarkAll
mkFFBookMarkAll =  FFBookMarkAll{ ttIdx = 0
                                , ffURLx = toSText ""
                                , ffTitlex = toSText ""
                                , dateAddedx = 0
                                , ffHashx = 0
                                }

selectItemsTitleHas::String -> String -> IO [FFurls]
selectItemsTitleHas s dbFile = do 
        conn <- open dbFile
        let ss = "%" ++ s ++ "%"::String
        let sql_select = Query {fromQuery = toSText qStr }
        urlInfo <- query conn sql_select (Only ss) :: IO [FFurls]
        return urlInfo
    where
      -- qStr = "DELETE FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%Vansky%'));"
      -- qStr = "SELECT id, url, hash FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%APL%'));"
      -- qStr = "SELECT id, url, hash FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE ? ));"
      qStr = "SELECT id, url, hash FROM urls WHERE id IN (SELECT urlId FROM items X WHERE title LIKE ? );"

selectURLHas::String -> String -> IO [FFurls]
selectURLHas s dbFile = do
        conn <- open dbFile
        let ss = "%" ++ s ++ "%"::String
        let sql_select = Query {fromQuery = toSText qStr }
        urlInfo <- query conn sql_select (Only ss) :: IO [FFurls]
        return urlInfo
    where
      -- qStr = "DELETE FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%Vansky%'));"
      -- qStr = "SELECT id, url, hash FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%APL%'));"
      qStr = "SELECT id, url, hash FROM urls WHERE url LIKE ? ;"

selectURLHasX::String -> String -> IO [FFBookMarkAll]
selectURLHasX s dbFile = queryBookMarkInfoX s "" dbFile 

{-|
    data FFurls = FFurls
      { ffId :: Int64
      , ffURL :: TS.Text
      , ffHash :: Int64
      } deriving (Eq,Read,Show)
-}
printURLInfo::[FFurls] -> IO()
printURLInfo u = do
        mapM_ (\x -> let uid = show $ ffId x
                         emp = " "
                         url = toStr $ ffURL x 
                     in putStrLn $ emp <> url <> emp <> "[" <> uid <> "]"
              ) u
      where
        (+) = (++)
{-|
    Delete row from urls table where title of TABLE(items) contains Str 
-}
deleteURLFromItemsTitleHas::String -> String -> IO () 
deleteURLFromItemsTitleHas s dbFile = do 
        conn <- open dbFile
        -- ss = "%Str%" 
        let ss = "%" ++ s ++ "%" :: String
        let sql_delete = Query {fromQuery = toSText "DELETE FROM urls WHERE id IN (SELECT urlId FROM items X WHERE title LIKE ? );"} 
        -- qStr = "DELETE FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%Vansky%'));"
        execute conn sql_delete (Only ss) 
        let delRowItem = Query {fromQuery = toSText "DELETE FROM ITEMS where urlId IS NULL;"}
        execute conn delRowItem () 
        return ()

deleteURLMatchURL::String -> String -> IO()
deleteURLMatchURL s dbFile = do
        conn <- open dbFile
        -- ss = "%Str%" 
        let ss = "%" ++ s ++ "%" :: String
        let sql_delete = Query {fromQuery = toSText "DELETE FROM urls WHERE url LIKE ?"} 
        -- qStr = "DELETE FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%Vansky%'));"
        execute conn sql_delete (Only ss) 
        let delRowItem = Query {fromQuery = toSText "DELETE FROM ITEMS where urlId IS NULL;"}
        execute conn delRowItem () 
        return ()


{-|
    TODO: Pass search str to the function

    @
    let sql_select = Query {fromQuery = toSText "SELECT id, url, hash FROM urls"}
    @ 
-}
queryBookMarkInfo::String -> IO[FFBookMarkAll]
queryBookMarkInfo dbFile = do
        conn <- open dbFile
        let sql_select = Query {fromQuery = toSText "SELECT id, url, hash FROM urls"}
        urlInfo <- query_ conn sql_select ::IO [FFurls]
        -- pre urlInfo
        let tit = toSText "ok"
        let qq = Query {fromQuery = toSText "SELECT urlId, title, dateAdded FROM items WHERE urlId = :urlId"}
        pp "test rows"
        ffbm <- mapM (\uinfo -> do
                          -- let id = ffId uinfo
                          let urlIdx = ffId uinfo :: Int64
                          let urlName = ffURL uinfo :: TS.Text
                          let hash = ffHash uinfo :: Int64
                          rowTitle <- queryNamed conn qq [ (toSText ":urlId") := urlIdx ]::IO[FFItems]
                          if (not . null) rowTitle then do
                              -- pp $ "hash=" ++ (sw hash)
                              -- pre urlName
                              -- pre rowTitle
                              let ffBookMarkAll = FFBookMarkAll{ ttIdx = urlIdx
                                                               , ffURLx = urlName
                                                               , ffTitlex = (ffTitle . head) rowTitle
                                                               , dateAddedx = (dateAdded . head) rowTitle
                                                               , ffHashx = ffHash uinfo
                                                               }
                              return ffBookMarkAll                                 
                            else do
                            -- pp $ (sw urlIdx) ++ " does not contains data"
                            return mkFFBookMarkAll
                             
             ) urlInfo
        fl
        -- pre ffbm
        -- pp $ "len=" ++ (sw. len) ffbm
        let ls = filter (\x -> ffHashx x /= 0) ffbm

        let deleteBuiltin = map toSText ["About Us", "Customize Firefox", "Get Involved", "Help and Tutorials"]
        let ls' = rms deleteBuiltin ls
              where
                rmTitle a cx = filter(\x -> ffTitlex x /= a) cx
        
                rms [] ss = ss
                rms (x:cs) ss = rms cs (rmTitle x ss)
                
        return ls'

queryBookMarkInfoX::String -> String -> String -> IO[FFBookMarkAll]
queryBookMarkInfoX us ts dbFile = do
        conn <- open dbFile
        let urlS = if (not . null) us then " WHERE url LIKE " + "'%" + us + "%';" else us 
        let titleS = if (not . null) ts then " AND title LIKE " + "'%" + ts + "%';" else ts
        let sql_select = Query {fromQuery = toSText $ " SELECT id, url, hash FROM urls" + urlS}
        urlInfo <- query_ conn sql_select ::IO [FFurls]
        -- pre urlInfo
        let tit = toSText "ok"
        let qq = Query {fromQuery = toSText $ "SELECT urlId, title, dateAdded FROM items WHERE urlId = :urlId " + titleS}
        pp "test rows"
        ffbm <- mapM (\uinfo -> do
                          -- let id = ffId uinfo
                          let urlIdx = ffId uinfo :: Int64
                          let urlName = ffURL uinfo :: TS.Text
                          let hash = ffHash uinfo :: Int64
                          rowTitle <- queryNamed conn qq [ (toSText ":urlId") := urlIdx ]::IO[FFItems]
                          if (not . null) rowTitle then do
                              -- pp $ "hash=" ++ (sw hash)
                              -- pre urlName
                              -- pre rowTitle
                              let ffBookMarkAll = FFBookMarkAll{ ttIdx = urlIdx
                                                               , ffURLx = urlName
                                                               , ffTitlex = (ffTitle . head) rowTitle
                                                               , dateAddedx = (dateAdded . head) rowTitle
                                                               , ffHashx = ffHash uinfo
                                                               }
                              return ffBookMarkAll                                 
                            else do
                            -- pp $ (sw urlIdx) ++ " does not contains data"
                            return mkFFBookMarkAll
                             
             ) urlInfo
        fl
        -- pre ffbm
        -- pp $ "len=" ++ (sw. len) ffbm
        let ls = filter (\x -> ffHashx x /= 0) ffbm

        let deleteBuiltin = map toSText ["About Us", "Customize Firefox", "Get Involved", "Help and Tutorials"]
        let ls' = rms deleteBuiltin ls
              where
                rmTitle a cx = filter(\x -> ffTitlex x /= a) cx
        
                rms [] ss = ss
                rms (x:cs) ss = rms cs (rmTitle x ss)
                
        return ls'
    where
      (+) = (++)

refillEmptyTitle:: String -> String -> String
refillEmptyTitle str u = if (null . trim) str then
                            if (upperStr $ takeEnd nChar u) == htmlExt then takeName $ dropEnd nChar u else takeName u
                         else str 
            where
                htmlExt = ".HTML"
                nChar = len htmlExt 


{-|
    data FFBookMarkAll = FFBookMarkAll
      { ttIdx :: Int64
      , ffURLx :: TS.Text
      , ffTitlex :: TS.Text
      , dateAddedx::Int64
      , ffHashx :: Int64
      } deriving (Eq,Read,Show)
-}
printBookMarkInfo::String -> IO()
printBookMarkInfo dbFile = do 
    ffBookMarkAll <- queryBookMarkInfo dbFile
    let ls = partList 30 ffBookMarkAll
    let ls' = (map) (\x -> zip [1..] x) ls
    mapM_ (\ss -> do
            mapM_ (\(n, bm) -> do
                            let url = toStr $ ffURLx bm 
                            let title = toStr $ ffTitlex bm 
                            let color = colorfgStr
                            let s = url + (red " → ") + title 
                            let s' = colorfgStr 2 s 
                            if mod n 2 == 0 then putStrLn s else putStrLn s'
                  ) ss 
            s <- getLine
            putStrLn s
            clear
            setCursorPos 10 1 
          ) ls'
    putStrLn $ " Len=" ++ (show $ len ffBookMarkAll)
  where
    (+) = (++)
    red = colorfgStr 9

printBookMarkInfoX::[FFBookMarkAll]-> IO()
printBookMarkInfoX ffBookMarkAll = do 
    let ls = partList 30 ffBookMarkAll
    let ls' = (map) (\x -> zip [1..] x) ls
    mapM_ (\ss -> do
            mapM_ (\(n, bm) -> do
                            let url = toStr $ ffURLx bm 
                            let title = toStr $ ffTitlex bm 
                            let color = colorfgStr
                            let s = url + (red " → ") + title 
                            let s' = colorfgStr 10 s 
                            if mod n 2 == 0 then putStrLn s else putStrLn s'
                  ) ss 
            s <- getLine
            putStrLn s
            clear
            setCursorPos 10 1 
          ) ls'
    putStrLn $ " Len=" ++ (show $ len ffBookMarkAll)
  where
    (+) = (++)
    red = colorfgStr 9


urlInfoToList::String -> IO [[String]]
urlInfoToList dbFile = do 
                ls <- queryBookMarkInfo dbFile 
                -- pre ls
                pp $ "new len=" ++ (sw . len) ls
                -- pre $ map ffId urlInfo
                -- pre $ map ffURL urlInfo
                let html = map (\x -> let title  = ffTitlex x
                                          url    = ffURLx x
                                          title' = refillEmptyTitle (toStr title) (toStr url)
                                          date   = dateAddedx x
                                          img    = toSText "<img src='svg/code.svg' alt='img' style='width:20px;height:20px;'>"
                                          -- htitle = st "<p>" <> title' <> st "</p>"
                                          href   = toSText "<a href='" <> url <> toSText "'>" <> img <> toSText title' <> toSText "</a><br>" 
                                        in (toSText title', (len . trim) title', href, date)
                               ) ls  -- [(title, href)]
                let sortedHtml = qqsort (\x y -> let
                                                   s1 = x^._1  -- fst (a, b)
                                                   s2 = y^._1  
                                                 in s1 < s2
                                        ) html
                
                let sortedHtml'= qqsort (\x y -> let
                                                   s1 = x^._4
                                                   s2 = y^._4
                                                 in s2 < s1
                                        ) sortedHtml
                -- pre sortedHtml
                let lsStr' = map (\x -> toStr (x^._3)) sortedHtml'
                let ps = partList 2 lsStr'
                -- let pt = htmlTable ps
                return ps 

helpMe::IO()
helpMe = do
    pr ["-h         → Help Menu"]
    pr ["-t str     → List title contains str"]
    pr ["-u str     → List URL contains str"]
    pr ["-du str    → Delete URL contains str"]
    pr ["-dt str    → Delete Title contains str"]
  where
    pr = printBox 2

main = do
        home <- getEnv "HOME"
        args <- getArgs
        let dbFile = bmfile home
        let ln = len args
        case ln of  
            var | var == 2 -> do
                    let opt = head args
                    let input = last args
                    case opt of
                         var | hasStr "-t" opt -> do
                                ps <- urlInfoToList dbFile
                                let pt = htmlTable ps 
                                htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"

                                writeFileList htmlFile pt
                                pp $ "Generate html => " ++ htmlFile


                                ffBookMarkAll <- queryBookMarkInfoX [] input dbFile
                                printBookMarkInfoX ffBookMarkAll 
                                -- uInfo <- selectItemsTitleHas input dbFile
                                -- printURLInfo uInfo

                                pre args
         
                             | hasStr "-u" opt -> do
                                ps <- urlInfoToList dbFile
                                let pt = htmlTable ps 
                                htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"

                                writeFileList htmlFile pt
                                pp $ "Generate html => " ++ htmlFile

                                ffBookMarkAll <- queryBookMarkInfoX input [] dbFile
                                printBookMarkInfoX ffBookMarkAll 

                                -- uInfo <- selectURLHas input dbFile
                                -- printURLInfo uInfo
                                pre args

                             | hasStr "-du" opt -> do
                                ps <- urlInfoToList dbFile
                                let pt = htmlTable ps 
                                htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"

                                writeFileList htmlFile pt
                                pp $ "Generate html => " ++ htmlFile
                                deleteURLMatchURL input dbFile
                                pre args

                             | hasStr "-dt" opt -> do
                                ps <- urlInfoToList dbFile
                                let pt = htmlTable ps 
                                htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"

                                writeFileList htmlFile pt
                                pp $ "Generate html => " ++ htmlFile
                                deleteURLFromItemsTitleHas input dbFile
                                pre args
                             | otherwise -> do
                                pp $ "Invalid Option => " ++ opt

        {-|            
                    if hasStr "-l" opt then do 
                        -- run "sqlite_copy_firefox_to_testfile.sh"
                        -- cpff -> /Users/aaa/myfile/bitbucket/script/sqlite_copy_firefox_to_testfile.sh
                        -- run "cpff"

                        ps <- urlInfoToList dbFile
                        let pt = htmlTable ps 
                        htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"

                        writeFileList htmlFile pt
                        pp $ "Generate html => " ++ htmlFile
                        pp "done!"
                        fw "uInfo"
                        uInfo <- selectItemsTitleHas input dbFile
                        printURLInfo uInfo
                        pre args
                     else if hasStr "-u" opt then do
                        ps <- urlInfoToList dbFile
                        let pt = htmlTable ps 
                        htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"

                        writeFileList htmlFile pt
                        pp $ "Generate html => " ++ htmlFile
                        pp "done!"
                        fw "uInfo"
                        uInfo <- selectURLHas input dbFile
                        printURLInfo uInfo
                        pre args
                     else do 
                       if hasStr "-dt" opt then do 
                         deleteURLFromItemsTitleHas input dbFile 
                         pp "kk"
                       else do
                         pp "ok"
        -}

                | var == 0 -> do 
                    ffBookMarkAll <- queryBookMarkInfo dbFile
                    printBookMarkInfoX ffBookMarkAll 
                | var == 1 -> do
                    let hs = head args
                    if hasStr "-h" hs then helpMe else pp "Invalid Option"
                | otherwise -> do
                    pp "Otherwise"
