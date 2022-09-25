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
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M 
import qualified Data.HashMap.Strict as MS

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
import           System.Console.Haskeline
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
  
-- BEG_FFurls
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
-- END_FFurls

-- BEG_FFurlsX
data FFurlsX = FFurlsX
  { ffIdxx :: Int64
  , ffURLxx :: TS.Text
  , ffTitlexx :: TS.Text
  , ffHashxx :: Int64
  } deriving (Eq,Read,Show)

-- | import Database.SQLite.Simple.FromRow
-- | two fields: shId, shcmd
instance FromRow FFurlsX where
   fromRow = FFurlsX <$> field <*>field <*> field <*> field

-- | import Database.SQLite.Simple.ToRow
instance ToRow FFurlsX where
   toRow (FFurlsX _ffIdxx ffURLxx ffTitlexx ffHashxx) = toRow (ffURLxx, ffTitlexx, ffHashxx)
-- END_FFurlsX


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

-- bmfile h = h </> "myfile/bitbucket/testfile/firefox_bookmark_test.sqlite"
-- bmfile h = "/Users/aaa/Library/Application Support/Firefox/Profiles/sk75a0xs.default-release-1/weave/bookmarks.sqlite"
bmfile h = h </> "Library/Application Support/Firefox/Profiles/sk75a0xs.default-release-1/places.sqlite"
backupDir = "dbbackup"

dbfile home = home </> "myfile/bitbucket/testfile/ShellHistory.db"
hisFile home = home </> "myfile/bitbucket/shell/dot_bash_history_test"

{-|
    --  root/config.txt/src
    os = Darwin
    -- /Users/aaa/Library/Application Support/Firefox/Profiles/sk75a0xs.default-release-1/places.sqlite
    -- places.sqlite is copied from Firefox App dir
    -- When Firefox is running, places.sqlite is locked. Other process CAN NOT open/access the db file.
    db_bookmark = /Users/aaa/myfile/github/notshare/places.sqlite
-}
configFile = "./config.txt"

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
deleteURLFromItemsTitleHas::String -> Connection -> IO () 
deleteURLFromItemsTitleHas s conn = do 
        -- ss = "%Str%" 
        let ss = "%" ++ s ++ "%" :: String
        let sql_delete = Query {fromQuery = toSText "DELETE FROM urls WHERE id IN (SELECT urlId FROM items X WHERE title LIKE ? );"} 
        -- qStr = "DELETE FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%Vansky%'));"
        execute conn sql_delete (Only ss) 
        let delRowItem = Query {fromQuery = toSText "DELETE FROM ITEMS where urlId IS NULL;"}
        execute conn delRowItem () 
        return ()



deleteURLMatchURL::String -> Connection -> IO()
deleteURLMatchURL s conn = do
        -- conn <- open dbFile
        -- ss = "%Str%" 
        let ss = "%" ++ s ++ "%" :: String
        -- let sql_delete = Query {fromQuery = toSText "DELETE FROM urls WHERE url LIKE ?"} 
        let sql_delete = Query {fromQuery = toSText "DELETE FROM moz_places WHERE url LIKE ?"} 
        -- qStr = "DELETE FROM urls WHERE id IN (SELECT id FROM urls U WHERE U.id IN (SELECT urlId FROM items X WHERE title LIKE '%Vansky%'));"
        execute conn sql_delete (Only ss) 
        -- let delRowItem = Query {fromQuery = toSText "DELETE FROM ITEMS where urlId IS NULL;"}
        -- execute conn delRowItem () 
        return ()

{-|
    * Delete row from urls table
    * Delete row from minimum bound and maximum bound from urls table

    @
    execute conn Query{fromQuery = toSText "DELETE FROM urls WHERE  ? <= id AND id <= ?;"} (minPid'::Int64, maxPid'::Int64)
    @
-}
deleteURLFromId::(Int, Int) -> Connection -> IO () 
deleteURLFromId (minPid,  maxPid) conn = do 
        -- conn <- open dbFile
        let minPid' = fromIntegral minPid :: Int64
        let maxPid' = fromIntegral maxPid :: Int64
        execute conn Query{fromQuery = toSText "DELETE FROM moz_places WHERE  ? <= id AND id <= ?;"} (minPid'::Int64, maxPid'::Int64)        
        
        -- let delRowItem = Query {fromQuery = toSText "DELETE FROM ITEMS where urlId IS NULL;"}
        -- execute conn delRowItem () 
        return ()

{-|
    * Delete row from urls table
    * Delete row from list of primary ids from urls table

    @
        let dbFile = /tmp/table.sql
        deleteURLAllId [1,2,3] dbFile
    @ 
-}
deleteURLAllId::[Int] -> Connection -> IO () 
deleteURLAllId cx conn = do 
        -- conn <- open dbFile
        mapM_ (\x -> do
                      let pid = fromIntegral x :: Int64
                      execute conn Query{fromQuery = toSText "DELETE FROM moz_places WHERE  id = ?;"} ((Only pid))        
              ) cx
        
        -- let delRowItem = Query {fromQuery = toSText "DELETE FROM ITEMS where urlId IS NULL;"}
        -- execute conn delRowItem () 
        return ()
{-|
    TODO: Pass search str to the function

    @
    let sql_select = Query {fromQuery = toSText "SELECT id, url, hash FROM urls"}
    @ 
-}
queryBookMarkInfo::Connection-> IO[FFBookMarkAll]
queryBookMarkInfo conn = do
        -- conn <- open dbFile
        let sql_select = Query {fromQuery = toSText "SELECT id, url, hash FROM urls"}
        urlInfo <- query_ conn sql_select ::IO [FFurls]
        -- pre urlInfo
        let tit = toSText "ok"
        let qq = Query {fromQuery = toSText "SELECT urlId, title, dateAdded FROM items WHERE urlId = :urlId"}
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

{-|
    Table urls and items
    INNER JOIN

    ORDER BY B.dateAdded ASC
                          ↑ 
                          + → as - cent 
    ORDER BY B.dateAdded DESC
                          ↑ 
                          + → des - cent
                            
-}
queryURLAndTitle::String -> String -> Connection -> IO[FFBookMarkAll]
queryURLAndTitle us ts conn = do 
        let ts' = if (not . null) ts then "%" + ts + "%" else "%"
        let us' = if (not . null) us then "%" + us + "%" else "%" 
        -- let qStr = Query {fromQuery = toSText $ "SELECT U.id, U.url, X.title, X.dateAdded, U.hash FROM urls U INNER JOIN items X ON U.id = X.urlId WHERE U.url LIKE ? AND X.title LIKE ? GROUP BY U.id;"}
        let qStr = Query {fromQuery = toSText $ "SELECT P.id, P.url, P.title, B.dateAdded, P.url_hash FROM moz_places P INNER JOIN moz_bookmarks B ON P.id = B.fk WHERE P.url LIKE ? AND P.title LIKE ? GROUP BY P.id ORDER BY B.dateAdded DESC;"}
        -- let qStr = Query {fromQuery = toSText $ "SELECT U.id, U.url, X.title, X.dateAdded, U.hash FROM urls U INNER JOIN items X ON U.id = X.urlId GROUP BY U.id;"}
        ffBookMarkAll <- query conn qStr (us', ts') ::IO [FFBookMarkAll]
        -- ffBookMarkAll <- query conn qStr () ::IO [FFBookMarkAll]
        return ffBookMarkAll 
    where
      (+) = (++)

{-|

    @
    data FFBookMarkAll = FFBookMarkAll
      { ttIdx :: Int64
      , ffURLx :: TS.Text
      , ffTitlex :: TS.Text
      , dateAddedx::Int64
      , ffHashx :: Int64
      } deriving (Eq,Read,Show)
    @
-}
queryURLNewest::Int -> Connection -> IO[FFBookMarkAll]
queryURLNewest n conn = do
        let n' = fromIntegral n :: Int64
        -- let qStr = Query {fromQuery = toSText $ "SELECT U.id, U.url, X.title, X.dateAdded, U.hash FROM urls U INNER JOIN items X ON U.id = X.urlId GROUP BY U.id ORDER BY X.dateAdded ASC LIMIT ?;"}
        let qStr = Query {fromQuery = toSText $ "SELECT P.id, P.url, P.title, B.dateAdded, P.url_hash FROM moz_places P INNER JOIN moz_bookmarks B ON P.id = B.fk GROUP BY P.id ORDER BY B.dateAdded DESC LIMIT ?;"}
        -- let qStr = Query {fromQuery = toSText $ "SELECT P.id, P.url, P.title, B.dateAdded, P.url_hash FROM moz_places P INNER JOIN moz_bookmarks B ON 2 = 2 LIMIT ?;"}
        -- let qStr = Query {fromQuery = toSText $ "SELECT U.id, U.url, X.title, X.dateAdded, U.hash FROM urls U INNER JOIN items X ON U.id = X.urlId GROUP BY U.id;"}
        -- ffBookMarkAll <- query conn qStr (Only(n)) ::IO [FFBookMarkAll]
        -- ffBookMarkAll <- query conn qStr (Only n) :: IO [(Int, TS.Text, TS.Text, Int, Int)]
        ffBookMarkAll <- query conn qStr (Only n):: IO [(Int, TS.Text, Maybe TS.Text, Int, Int)]
        ffb <- forM ffBookMarkAll $ \(pid, url, title, dateAdded, url_hash) ->  do
            -- putStrLn $ "id=" ++ (show pid) 
            putStrLn "forM_"
            let ffb = FFBookMarkAll{
                        ttIdx = fromIntegral pid,
                        ffURLx = url,
                        ffTitlex = case title of
                                        Just x -> x
                                        Nothing -> toSText "" 
                                   ,
                        dateAddedx = 0,
                        ffHashx = 0
                      }
            return ffb
--            case title of
--                Just x -> putStrLn "title"
--                Nothing -> putStrLn "nothing"
        -- ffBookMarkAll <- query conn qStr () ::IO [FFBookMarkAll]
        -- return ffBookMarkAll 
        return [mkFFBookMarkAll]
        return ffb

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
printBookMarkInfo::Connection -> IO()
printBookMarkInfo conn = do 
    ffBookMarkAll <- queryBookMarkInfo conn 
    let ls = partList 20 ffBookMarkAll
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
            setCursorPos 10 0 
          ) ls'
    putStrLn $ " Len=" ++ (show $ len ffBookMarkAll)
  where
    (+) = (++)
    red = colorfgStr 9

isAllDigit::String -> Bool 
isAllDigit s = len s > 0 && (foldr (\a b -> a && b) True $ map (\x -> isDigit x) s)


printURL::[(Int, Int, String, String)] -> IO()
printURL ts = do
    mapM_ (\(ix, pId, url, title) -> do
                                        let color = colorfgStr
                                        let s = url + (red " → ") + title 
                                        let s' = colorfgStr 10 s 
                                        let pix = let cx = show ix in len cx == 1 ? cx + " " $ cx
                                        putStr $ "[" + pix + "] [" + (show pId) + "]"
                                        if mod ix 2 == 0 then putStrLn s else putStrLn s'
          ) ts
  where
   red = colorfgStr 9
   (+) = (++)

loopPage:: [(Int, FFBookMarkAll)] -> String -> Connection -> IO() 
loopPage ss cmd conn = do
    clear


    let urlTuple = map (\(ix, bm) -> let pId = (fromIntegral . ttIdx) bm :: Int
                                         url = toStr $ ffURLx bm 
                                         title = toStr $ ffTitlex bm 
                                     in  (ix, pId, url, title)
                   ) ss


    setCursorPos 6 0 
    putStrLn $ toRightStr 10 ++ "Len=" + (show . len) ss

    setCursorPos 12 0 

    let kvMap = M.fromList $ map (\(ix, pId, _, _) -> (ix, pId)) urlTuple 
    printURL urlTuple
    putStrLn $ "Len=" + (show . len) ss
    pp "loopPage => getLine"
    s <- getLine 
    case s of
        var | hasPrefix "-de" var -> do
                let arr = splitSPC var 
                pre arr
                let ln = len arr
                case ln of
                    v | v == 2 -> do
                        -- Delete one Pid from TABLE urls
                        -- DELETE FROM urls where id = 12;
                        -- -tt 12 
                        let opt = head arr
                        let qst = last arr
                        let inx = read qst :: Int64
                        let pid = fromJust $ M.lookup (fromIntegral inx) kvMap
                        pp arr
                        deleteURLFromId (pid, pid) conn 
                        pp ""
                      | v == 3 -> do
                        -- Delete Pid from 1 to 2 from TABLE urls
                        -- DELETE FROM urls where 1 <= id AND id <= 2;
                        -- -tt 1 2
                        let opt = head arr  
                        let minPid = case M.lookup (read $ (head . tail) arr :: Int) kvMap  of
                                          Just x  -> x
                                          Nothing -> error $ "Invalid minPid=" ++ (show arr) 

                        let maxPid = case M.lookup (read $ last arr :: Int) kvMap  of
                                          Just x  -> x
                                          Nothing -> error $ "Invalid maxPid=" ++ (show arr)

                        pp $ "minPid=" + (show minPid)
                        pp $ "maxPid=" + (show maxPid)
                        pp arr
                        deleteURLFromId (minPid, maxPid) conn 

                      | otherwise -> do
                        pp "otherwise"
                loopPage ss cmd conn 
            | hasPrefix "-dall" var -> do
                let arr = splitSPC var
                pre arr
                if len arr > 1 then do
                    let ls = filter (>= 0) $ map (\x -> case let n = read x :: Int 
                                                             in M.lookup n kvMap of
                                                                 Just k -> k
                                                                 Nothing -> -1
                                                 ) $ tail arr
                    pp "len arr > 1"
                    pre ls
                    deleteURLAllId ls conn 
                    pp ""
                else do
                    pp "Not len arr > 1"
                pp ""
            | hasPrefix "-t" var -> do
                -- Title matches input
                pp $ "input -t =>" ++ var 
                let input = last $ splitSPC var
                pp var 
                ffBookMarkAll <- queryURLAndTitle [] input conn 
                let tt = zip [0..] ffBookMarkAll
                loopPage tt cmd conn 
                pp ""

            | hasPrefix "-u" var -> do
                -- Title matches input
                pp $ "input -u =>" ++ var 
                let input = last $ splitSPC var
                pp var 
                ffBookMarkAll <- queryURLAndTitle input [] conn 
                let tt = zip [0..] ffBookMarkAll
                loopPage tt cmd conn 
                pp ""

            | hasPrefix "-ne" var -> do
                let s = last $ splitSPC var
                let n = read s :: Int
                ffBookMarkAll <- queryURLNewest n conn 
                let tt = zip [0..] ffBookMarkAll
                loopPage tt cmd conn 

            | hasStr "-du" var -> do
                let s = last $ splitSPC var
                deleteURLMatchURL s conn 
                ps <- urlInfoToList conn 
                -- let pt = htmlTable ps 
                -- writeFileList htmlFile pt
                pp ""

            | hasStr "-dt" var -> do
                let s = last $ splitSPC var
                deleteURLFromItemsTitleHas s conn 
                ps <- urlInfoToList conn 
                pp ""
                -- let pt = htmlTable ps 
                -- writeFileList htmlFile pt

--            | hasPrefix "n" var -> do     
--                let s = last $ splitSPC var
--                let n = read s :: Int
--                ffBookMarkAll <- queryURLNewest n dbFile
--                let tt = zip [0..] ffBookMarkAll
--                loopPage tt cmd dbFile

            | isAllDigit var -> do
                -- Open Browser from url
                let inx = read var ::Int
                if inx < len ss then do
                    putStrLn $ "inx=" + sw inx
                    let ffbm = (map snd ss) ! inx 
                    let title = (toStr . ffTitlex) ffbm 
                    let url = (toStr . ffURLx) ffbm 
                    putStrLn $ "title=" + title + " " + url 
                    sys $ "open " + url

                    loopPage ss cmd conn 
                else do
                    pp $ "Invalid Index => 999" + sw inx
                    loopPage ss cmd conn 

            | otherwise -> do
                pp $ "Invalid Option 444" ++ var
    return ()
  where
   (+) = (++)
   (!) = (!!)
   red = colorfgStr 9
   sw = show

iterateBookMark::[FFBookMarkAll] -> String -> IO()
iterateBookMark ffBookMarkAll dbFile = do 
    let ls = partList 20 ffBookMarkAll
    let ls' = map (\x -> zip [0..] x) ls
    let lst' = zip [0..] ls'
    mapM_ (\(k, ss) -> do

            -- loopPage ss dbFile 
            putStrLn $ "Page=" + (show k) 

          ) lst'
    putStrLn $ "Len=" ++ (show $ len ffBookMarkAll)
  where
    (+) = (++)
    (!) = (!!)
    red = colorfgStr 9


cursorUpLine::Int -> IO()
cursorUpLine n = do
  let s = "\x1b[" + (show n) + "A"
  putStr s
 where
   (+) = (++)
  
cursorDownLine::Int -> IO()
cursorDownLine n = do
  let s = "\x1b[" + (show n) + "B"
  putStr s
 where
   (+) = (++)
  
{-|
   Move cursor to Line #, Column #
-}
cursorToLC::Int -> Int -> IO()
cursorToLC ln cn = do
  let s = "\x1b[" + (show ln) + ";" + (show cn) + "H"
  --                  ↑                  ↑ 
  --                  + → line #         + → column #
  putStr s
 where
   (+) = (++)

cursorToColumn::Int -> IO()
cursorToColumn n = do
  let s = "\x1b[" + (show n) + "G"
  putStr s
 where
   (+) = (++)
  
cursorToRightCol::Int -> IO()
cursorToRightCol n = do
  let s = "\x1b[" + (show n) + "C"
  putStr s
 where
   (+) = (++)

{-|
    Move cursor to the right of n columns relative to current position
-}
toRightStr::Int -> String  
toRightStr n = "\x1b[" ++ (show n) ++ "C"

{-|
    Move cursor to column #
-}
moveToColStr::Int -> String
moveToColStr n = "\x1b[" ++ (show n) ++ "G"

{-|
    Move cursor to the left of n columns
-}
toLeft::Int -> String
toLeft n = "\x1b[" ++ (show n) ++ "D"

rightCol::Int -> String 
rightCol n = "\x1b[" + (show n) + "C"
 where
   (+) = (++)
  
cursorToLeftCol::Int -> IO()
cursorToLeftCol n = do
  let s = "\x1b[" + (show n) + "D"
  putStr s
 where
   (+) = (++)
  
mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "myhist"}

{-|
    === KEY: backup file

    @
    Backup file:
    places.sqlite ⟹  places.sqlite_2022-09-14_00_11_07_081807_PDT

    backup "/tmp/x.txt"  "/tmp" => "/tmp/x.txt_2022-09-14_00_11_07_081807_PDT
    @
-}
backup::FilePath -> FilePath -> IO () 
backup dbFile newDir = do
    s <- dateStr >>= \x -> return $ concatStr (splitSPC x) "_"
    let s' = replaceRegex (mkRegex ":|\\.") s "_"
    home <- getEnv "HOME"
    let newName = (takeName dbFile) ++ "_" ++ s'
    let dbFile' = "\"" ++ dbFile ++ "\""
    b <- fileExistA newDir 
    when (not b) $ do 
        mkdir newDir
    cp dbFile (newDir </> newName)

iterateIndex::[[FFBookMarkAll]] -> Int -> [String] -> Connection -> IO()
iterateIndex lsbm ix lsMsg conn = do
    clear
    setCursorPos 10 0 

    let pageBookMark = isBound lsbm ix ? lsbm ! ix $ []
    -- pre pageBookMark

    -- Processing page here
    let tuplePage = zip [0..] pageBookMark
    

    let urlTuple = map (\(ix, bm) -> let pId = (fromIntegral . ttIdx) bm :: Int
                                         url = toStr $ ffURLx bm 
                                         title = toStr $ ffTitlex bm 
                                     in  (ix, pId, url, title)
                   ) tuplePage

    let kvMap = M.fromList $ map (\(x, pid, _, _) -> (x, pid)) urlTuple 

    (nline, ncol) <- getTerminalSize
    let cenLine   = div nline 2 
    let cenColumn = div ncol 2
    setCursorPos 6 cenColumn 
    putStrLn $ (moveToColStr cenColumn) ++ "All Pages| " ++ (show . len) lsbm 
    putStrLn $ (moveToColStr cenColumn) ++ "Curr Page| " ++ (show (ix + 1)) 

    setCursorPos 12 0 

    printURL urlTuple
    cursorDownLine 5 

    when (not . null $ lsMsg) $ do
        mapM_ putStrLn lsMsg
    
    -- setCursorPos 30 5 
    -- let lc ln cn = "\x1b[" ++ (show ln) ++ ";" ++ (show cn) ++ "H"

    putStrLn $ setCursorPosStr 50 5
    putStr "ENTER "
    cmd <- getLineFlush
    -- loopPage tuplePage cmd conn 

    let ls = splitSPC cmd 
    case len ls of
        ln | ln == 1 -> do
                let opt = head ls
                case opt of
                    opt | hasPrefix "-h" opt -> do
                            home <- getEnv "HOME"
                            let msg = let s1  = [ri ++ "-h           → Help Menu"]
                                          s2  = [ri ++ "-t str       → List title contains str"]
                                          s3  = [ri ++ "-u str       → List URL contains str"]
                                          s4  = [ri ++ "-du str      → Delete URL contains str"]
                                          s5  = [ri ++ "-dall 1 2 3  → Delete all 1 2 3 rows"]
                                          s6  = [ri ++ "-de 1        → Delete one rows"]
                                          s7  = [ri ++ "-dr 1 3      → Delete rows from 1 to 3"]
                                          s8  = [ri ++ "-ne 10       → Query the newest 10 rows"]
                                          s9  = [ri ++ "all          → Show all"]
                                          s10 = [ri ++ "n            → Next page"]
                                          s11 = [ri ++ "p            → Previous page"]
                                          s12 = [ri ++ "backup dir   → " ++ backupDir]
                                          s13 = [ri ++ "db path"]
                                          s14 = [ri ++ "UPDATE: Sat 24 Sep 23:11:22 2022"]
                                          ri = toRightStr 10
                                      in s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6 ++ s7 ++ s8 ++ s9 ++ s10 ++ s11 ++ s12 ++ s13 ++ s14

                            iterateIndex lsbm ix msg conn 
                    opt | hasPrefix "n" opt -> do
                            pp $ "Next Page (ix + 1) =>" ++ (show $ ix + 1)
                            if (ix + 1 < len lsbm) then do
                                iterateIndex lsbm (ix + 1) [] conn 
                            else do    
                                iterateIndex lsbm ix [] conn 

                    opt | hasPrefix "p" opt -> do
                            pp $ "Previous Page (ix - 1) =>" ++ (show $ ix - 1)
                            if (0 <= ix - 1) then do
                                iterateIndex lsbm (ix - 1) [] conn 
                            else do
                                iterateIndex lsbm ix [] conn 

                        | hasPrefix "all" opt -> do
                            ffBookMarkAll <- queryURLAndTitle [] [] conn 
                            let lss = partList 20 ffBookMarkAll
                            -- iterateBookMark ffBookMarkAll dbFile
                            iterateIndex lss 0 [] conn 

                        | isAllDigit opt -> do
                            -- Open Browser from url
                            let inx = read opt ::Int
                            if inx < len pageBookMark then do
                                putStrLn $ "inx=" ++ sw inx

                                let ffbm = pageBookMark ! inx 
                                let title = (toStr . ffTitlex) ffbm 
                                let url = (toStr . ffURLx) ffbm 

                                putStrLn $ "title=" ++ title ++ " " ++ url 
                                sys $ "open " ++ url
                                iterateIndex lsbm ix [] conn
                                -- loopPage ss cmd conn 
                            else do
                                pp $ "Invalid Index => 999" ++ sw inx
                                iterateIndex lsbm ix [] conn

                        | hasPrefix "q" opt -> do 
                            -- quit
                            return ()
                        | otherwise -> do
                            pp $ "Invalid 1 argument ix =>" ++ (show ix)
                            iterateIndex lsbm ix [] conn 
           | ln == 2 -> do
                let opt = head ls
                case opt of
                    opt | hasPrefix "-u" opt -> do
                            -- URL matches input
                            pp $ "input -u =>" ++ opt 
                            let input = last ls 
                            ffBookMarkAll <- queryURLAndTitle input [] conn 
                            let lsffBookMarkAll = partList 20 ffBookMarkAll
                            iterateIndex lsffBookMarkAll 0 [] conn 

                        | hasPrefix "-t" opt -> do
                            -- Title matches input
                            pp $ "input -u =>" ++ opt 
                            let input = last ls 
                            ffBookMarkAll <- queryURLAndTitle [] input conn 
                            let lsffBookMarkAll = partList 20 ffBookMarkAll
                            iterateIndex lsffBookMarkAll 0 [] conn 

                        | hasPrefix "-du" opt -> do
                            -- Delete URL matches input
                            let input = last ls 
                            deleteURLMatchURL input conn 
                            ffBookMarkAll <- queryURLAndTitle input [] conn 
                            let lsffBookMarkAll = partList 20 ffBookMarkAll
                            iterateIndex lsffBookMarkAll 0 [] conn 

                        | hasStr "-dt" opt -> do
                            -- Delete Title matches input
                            let input = last ls 
                            deleteURLFromItemsTitleHas input conn 
                        | hasStr "-de" opt -> do
                            -- Delete one row
                            let n = last ls 
                            let inx = read n :: Int64
                            let pid = fromJust $ M.lookup (fromIntegral inx) kvMap
                            deleteURLFromId (pid, pid) conn 


                        | hasPrefix "-ne" opt -> do
                            -- Query the newest n rows from urls table
                            let s = last ls 
                            let n = read s :: Int
                            ffBookMarkAll <- queryURLNewest n conn 
                            let lsffBookMarkAll = partList 20 ffBookMarkAll
                            iterateIndex lsffBookMarkAll 0 [] conn 

                        | otherwise -> do
                            pp "ow"
                iterateIndex lsbm ix [] conn
           | ln == 3 -> do
                let opt = head ls
                case opt of
                    opt | hasPrefix "-dall" opt -> do
                            let da = filter (>= 0) $ map (\x -> case let n = read x :: Int 
                                                                     in M.lookup n kvMap of
                                                                         Just k -> k
                                                                         Nothing -> -1
                                                         ) $ tail ls 
                            pp "len arr > 1"
                            pre da 
                            deleteURLAllId da conn 
                            iterateIndex lsbm ix [] conn
                        | hasPrefix "-dr" opt -> do
                            let minPid = case M.lookup (read $ (head . tail) ls :: Int) kvMap  of
                                              Just x  -> x
                                              Nothing -> error $ "Invalid minPid=" ++ (show ls) 

                            let maxPid = case M.lookup (read $ last ls :: Int) kvMap  of
                                              Just x  -> x
                                              Nothing -> error $ "Invalid maxPid=" ++ (show ls)

                            pp $ "minPid=" ++ (show minPid)
                            pp $ "maxPid=" ++ (show maxPid)
                            logFileG ["show midPid=" ++ (show minPid), "maxPid=" ++ (show maxPid)]
                            let swapT (a, b) = (b, a)
                            deleteURLFromId (swapT (minPid, maxPid)) conn 
                            --                 ↑           ↑  
                            --                 |           + →  ORDER BY dateAdded DESC 
                            --                 + - - - - - + →  Need to swap (minPid, maxPid)
                            --        Pid
                            --   1 -> 003
                            --   2 -> 002
                            --   3 -> 001
                            --
                            -- SELECT P.id, P.url, P.title, B.dateAdded, P.url_hash FROM moz_places P INNER JOIN moz_bookmarks B ON P.id = B.fk WHERE P.url LIKE ? AND P.title LIKE ? GROUP BY P.id ORDER BY B.dateAdded DESC;
                            -- 
                            iterateIndex lsbm ix [] conn 

                        | otherwise -> do
                            pp "Invalid options"
                            iterateIndex lsbm ix [] conn 


           | otherwise -> do
                iterateIndex lsbm ix [] conn 
                pp "xx555"
  where
    (!) = (!!)
    isBound ls ix = 0 <= ix && ix < len ls

urlInfoToList::Connection -> IO [[String]]
urlInfoToList conn = do 
                ls <- queryBookMarkInfo conn 
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
    pr ["-h         → Help Menukk"]
    pr ["-t str     → List title contains str"]
    pr ["-u str     → List URL contains str"]
    pr ["-du str    → Delete URL contains str"]
    pr ["-dt str    → Delete Title contains str"]
  where
    pr = printBox 2


main = do
        args <- getArgs
        pre args
        home <- getEnv "HOME"
        let backupDir = home </> "myfile/github/notshare/firefox_bookmark_db"
        htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"
        osName <- getOS  -- "macOS => Darwin"
        pre osName
        confMap <- readConfig configFile
        let mayMap = MS.lookup osName confMap
        case mayMap of
            Nothing -> error "Can find OS name"
            Just maydbPath -> do 
                case MS.lookup "db_bookmark" maydbPath of
                    Nothing -> error "Can not find firefox bookmark sqlite file path => Please check your config.txt file."
                    Just dbFile -> do
                        pp $ "Generate html => " ++ htmlFile
                        backup dbFile backupDir
                        pre dbFile
                        let ln = len args
                        -- let inputFunc = getInputLine
                        conn <- open dbFile
                        ffBookMarkAll <- queryURLAndTitle [] [] conn 
                        let ls = partList 20 ffBookMarkAll
                        let lsMsg = [dbFile]
                        iterateIndex ls 0 [] conn 
{-|
        case mayMap of
            Nothing -> error "config.txt does not contain a valid os name such as Darwin, Linux etc."
            Just osMap -> do
                case MS.lookup "db_bookmark" osMap of
                    Nothing -> error "Can not find firefox bookmark sqlite file path => Please check your config.txt file."
                    Just dbFile -> do
                        pp $ "Generate html => " ++ htmlFile
                        backup dbFile backupDir
                        pre dbFile
                        when True $ do
                            let ln = len args
                            -- let inputFunc = getInputLine
                            conn <- open dbFile
                            ffBookMarkAll <- queryURLAndTitle [] [] conn 
                            let ls = partList 20 ffBookMarkAll
                            iterateIndex ls 0 [] conn 
-}









