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
### Change Sqlite3 table to: places.sqlite 
``` sql
    bmfile h = h </> "Library/Application Support/Firefox/Profiles/sk75a0xs.default-release-1/places.sqlite"
```
* Delete One URL using index
* Delete Range of URLs using index
* Search URL name
* Search Title name
* Delete URL name contains matched pattern
* Delete Title name contains matched pattern



