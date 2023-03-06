# haskell-bookmark-manager
# 
### sqlite database table
	* urls
	* items

## Firefox bookmark tables
## Location on macOS Big Sur 11.5.2

	* `~/Library/Application Support/Firefox/Profiles/sk75a0xs.default-release-1/weave/bookmarks.sqlite`
	* There are mainly two tables *urls* and *items* which contains url and title

### Sqlite table: items

```sql
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
  );

CREATE INDEX itemURLs ON items(urlId);
CREATE INDEX itemKeywords ON items(keyword)
                    WHERE keyword NOT NULL;

```

* We only use *id*, *dateAdded*, *urlId* from *items* table and ignore the rest
### Sqlite table: urls


``` sql
CREATE TABLE urls(
    id INTEGER PRIMARY KEY,
    guid TEXT NOT NULL,
    url TEXT NOT NULL,
    hash INTEGER NOT NULL,
    revHost TEXT NOT NULL
  );
CREATE INDEX urlHashes ON urls(hash);
```
* Similarily above, we only use *id*, *url* and *hash* from urls table

## Add package lens to cabal file, use len to access item in tuple
``` haskell
	:set -package lens
	import Control.Lens
	>(1, 2, 3)^._3  -- 3
```
## What I learned today?
### You can not use annotation like the following

``` haskell
    import Data.Text
	main = do
	       let ls = ["a"]::Text
		   print "ok"
		   
    -- You get compile error
	-- You have to convert String to Text with function
	-- "" only for String
```

## Update:
* file:///Users/aaa/myfile/github/notshare/bookmark.html
* Add: sorted title
* Add: sorted date
* Add: svg icon to text title
* Add: filter to remove some useless *title* from Firefox

## Update Mon 12 Sep 00:54:17 2022 
### Change Sqlite3 database to: places.sqlite  :.table → moz_places and moz_bookmarks
``` sql
    bmfile h = h </> "Library/Application Support/Firefox/Profiles/sk75a0xs.default-release-1/places.sqlite"

    CREATE TABLE moz_places (   id INTEGER PRIMARY KEY, url LONGVARCHAR, title LONGVARCHAR, rev_host LONGVARCHAR, visit_count INTEGER DEFAULT 0, hidden INTEGER DEFAULT 0 NOT NULL, typed INTEGER DEFAULT 0 NOT NULL, frecency INTEGER DEFAULT -1 NOT NULL, last_visit_date INTEGER , guid TEXT, foreign_count INTEGER DEFAULT 0 NOT NULL, url_hash INTEGER DEFAULT 0 NOT NULL , description TEXT, preview_image_url TEXT, origin_id INTEGER REFERENCES moz_origins(id));

    CREATE TABLE moz_bookmarks (  id INTEGER PRIMARY KEY, type INTEGER, fk INTEGER DEFAULT NULL, parent INTEGER, position INTEGER, title LONGVARCHAR, keyword_id INTEGER, folder_type TEXT, dateAdded INTEGER, lastModified INTEGER, guid TEXT, syncStatus INTEGER NOT NULL DEFAULT 0, syncChangeCounter INTEGER NOT NULL DEFAULT 1);
```
* Delete one URL using index
* Delete range of URLs using index
* Search URL name
* Search title name
* Delete URL name contains matched pattern
* Delete title name contains matched pattern

## Add config.txt file for sqlite database location
```
    $g/notshare/*
```

## Update: Sun  5 Mar 20:23:59 2023 
### Added search on URL OR Title
### Fixed bug on the Sqlite3 query 


``` haskell
  -- Wrong Query
  qs = "SELECT P.id, P.url, P.title, B.dateAdded, P.url_hash FROM moz_places P INNER JOIN moz_bookmarks B ON P.id = B.fk WHERE (P.url IS NOT NULL AND P.url LIKE ?) OR (P.title LIKE ?) GROUP BY P.id ORDER BY B.dateAdded DESC;"

  P.title => B.title

  -- Fixed Query
  qs = "SELECT P.id, P.url, B.title, B.dateAdded, P.url_hash FROM moz_places P INNER JOIN moz_bookmarks B ON P.id = B.fk WHERE (P.url IS NOT NULL AND P.url LIKE ?) OR (B.title LIKE ?) GROUP BY P.id ORDER BY B.dateAdded DESC;"

```

* Both table `moz_places` and `moz_bookmarks` contain `title`
* `title` in `moz_bookmarks` should be USED, Not in `moz_places`
```sql
CREATE TABLE moz_places (   id INTEGER PRIMARY KEY, url LONGVARCHAR, title LONGVARCHAR, rev_host LONGVARCHAR, visit_count INTEGER DEFAULT 0, hidden INTEGER DEFAULT 0 NOT NULL, typed INTEGER DEFAULT 0 NOT NULL, frecency INTEGER DEFAULT -1 NOT NULL, last_visit_date INTEGER , guid TEXT, foreign_count INTEGER DEFAULT 0 NOT NULL, url_hash INTEGER DEFAULT 0 NOT NULL , description TEXT, preview_image_url TEXT, origin_id INTEGER REFERENCES moz_origins(id), site_name TEXT);

CREATE TABLE moz_bookmarks (  id INTEGER PRIMARY KEY, type INTEGER, fk INTEGER DEFAULT NULL, parent INTEGER, position INTEGER, title LONGVARCHAR, keyword_id INTEGER, folder_type TEXT, dateAdded INTEGER, lastModified INTEGER, guid TEXT, syncStatus INTEGER NOT NULL DEFAULT 0, syncChangeCounter INTEGER NOT NULL DEFAULT 1);

```

