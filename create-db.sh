sqlite3 scrape.db "CREATE TABLE IF NOT EXISTS url (url text PRIMARY KEY, timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, visited integer DEFAULT 0);"
sqlite3 scrape.db "CREATE TABLE IF NOT EXISTS content (url text PRIMARY KEY, content text, timestamp DATETIME DEFAULT CURRENT_TIMESTAMP);"
sqlite3 scrape.db "CREATE TABLE IF NOT EXISTS dupe (hash integer PRIMARY KEY, recent_url text, timestamp DATETIME DEFAULT CURRENT_TIMESTAMP);"
sqlite3 scrape.db "CREATE TABLE IF NOT EXISTS like (hash integer PRIMARY KEY, url text, content text, note text, timestamp DATETIME DEFAULT CURRENT_TIMESTAMP);"
