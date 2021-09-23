# haskell-bookmark-manager
# 
### sqlite database table
* urls
* items

## Firefox bookmark tables
### Location on macOS Big Sur 11.5.2
* `~/Library/Application Support/Firefox/Profiles/sk75a0xs.default-release-1/weave/bookmarks.sqlite`
* There are mainly two tables *urls* and *items* which contains url and title<br>
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
<br>
* We only use *id*, *dateAdded*, *urlId* from *items* table and ignore the rest <br>
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

## Update:
* file:///Users/aaa/myfile/github/notshare/bookmark.html
* Add: sorted title
