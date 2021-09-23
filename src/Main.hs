-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
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
  
p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"
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
    urlId INTEGER REFERENCES urls(id)
                  ON DELETE SET NULL,
    keyword TEXT,
    description TEXT,
    loadInSidebar BOOLEAN,
    smartBookmarkName TEXT,
    feedURL TEXT,
    siteURL TEXT
j  );
CREATE INDEX itemURLs ON items(urlId);
CREATE INDEX itemKeywords ON items(keyword)
                    WHERE keyword NOT NULL;
CREATE TABLE urls(
    id INTEGER PRIMARY KEY,
    guid TEXT NOT NULL,
    url TEXT NOT NULL,
    hash INTEGER NOT NULL,
    revHost TEXT NOT NULL
  );
CREATE INDEX urlHashes ON urls(hash);


urlId INTEGER REFERENCES urls(id)
              ON DELETE SET NULL,
=>

If row in urls is deleted from urls table, then
uriId will be set to NULL in items table

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
  
queryBookMarkInfo::String -> IO[FFBookMarkAll]
queryBookMarkInfo dbFile = do
        conn <- open dbFile
        let sql_select = Query {fromQuery = toSText "SELECT id, url, hash FROM urls"}
        urlInfo <- query_ conn sql_select ::IO [FFurls]
        pre urlInfo
        let tit = toSText "ok"
        let qq = Query {fromQuery = toSText "SELECT urlId, title, dateAdded FROM items WHERE urlId = :urlId"}
        pp "test rows"
        ffbm <- mapM (\urlinfo -> do
                          -- let id = ffId urlinfo
                          let urlIdx = ffId urlinfo :: Int64
                          let urlName = ffURL urlinfo :: TS.Text
                          let hash = ffHash urlinfo :: Int64
                          rowTitle <- queryNamed conn qq [ (toSText ":urlId") := urlIdx ]::IO[FFItems]
                          if (not . null) rowTitle then do
                              pp $ "hash=" ++ (sw hash)
                              pre urlName
                              pre rowTitle
                              let ffBoobMarkAll = FFBookMarkAll{ ttIdx = urlIdx
                                                               , ffURLx = urlName
                                                               , ffTitlex = (ffTitle . head) rowTitle
                                                               , dateAddedx = (dateAdded . head) rowTitle
                                                               , ffHashx = ffHash urlinfo
                                                               }
                              return ffBoobMarkAll                                 
                            else do
                            pp $ (sw urlIdx) ++ " does not contains data"
                            return mkFFBookMarkAll
                             
             ) urlInfo
        fl
        pre ffbm
        pp $ "len=" ++ (sw. len) ffbm
        let ls = filter (\x -> ffHashx x /= 0) ffbm
        return ls

refillEmptyTitle:: String -> String -> String
refillEmptyTitle t u = if (null . trim) t then
                         if (upperStr $ takeEnd 5 u) == ".HTML" then takeName $ dropEnd 5 u else takeName u
                       else
                         t

                    
main = do
        home <- getEnv "HOME"
        -- run "sqlite_copy_firefox_to_testfile.sh"
        run "cpff"
        let dbFile = bmfile home
        ls <- queryBookMarkInfo dbFile 
        pre ls
        pp $ "new len=" ++ (sw . len) ls
        -- pre $ map ffId urlInfo
        -- pre $ map ffURL urlInfo
        let st = toSText
        let html = map (\x -> let title  = ffTitlex x
                                  url    = ffURLx x
                                  title' = refillEmptyTitle (toStr title) (toStr url)
                                  -- htitle = st "<p>" <> title' <> st "</p>"
                                  href   = st "<a href='" <> url <> st "'>" <> st title' <> st "</a><br>" 
                                in (st title', (length . trim) title', href)
                       ) ls  -- [(title, href)]
        let sortedHtml = qqsort (\x y -> let
                                           s1 = t1 x
                                           s2 = t1 y
                                         in s1 < s2
                                ) html
        pre sortedHtml
        let lsStr' = map (toStr . t3) sortedHtml
        let ps = partList 2 lsStr'
        let pt = htmlTable ps
        htmlFile <- getEnv "g" >>= \x -> return $ x </> "notshare/bookmark.html"
        writeFileList htmlFile pt
        pp $ "Generate html => " ++ htmlFile
        pp "done!"
