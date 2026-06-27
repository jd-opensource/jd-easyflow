-- SQLite database schema for lock module

CREATE TABLE lock_record (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    lock_key TEXT,
    lock_flag TEXT,
    request_id TEXT,
    expired_time INTEGER,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE UNIQUE INDEX uni_idx_lock_key ON lock_record(lock_key);

-- Trigger: auto-update modified_date on row update (replacement for MySQL ON UPDATE CURRENT_TIMESTAMP)
CREATE TRIGGER update_lock_record_modtime
AFTER UPDATE ON lock_record
FOR EACH ROW
BEGIN
    UPDATE lock_record SET modified_date = datetime('now') WHERE id = NEW.id;
END;
