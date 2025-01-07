-- CREATE TABLE IF NOT EXISTS log (
--     id INTEGER PRIMARY KEY AUTOINCREMENT,
--     content TEXT NOT NULL
-- );

DROP TABLE IF EXISTS log;

CREATE TABLE IF NOT EXISTS log (
    id TEXT PRIMARY KEY NOT NULL,
    content TEXT NOT NULL
);
