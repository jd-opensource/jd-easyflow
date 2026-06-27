-- SQLite database schema for codegenerator module

CREATE TABLE sequence (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    seq_key TEXT,
    seq_sub_key TEXT,
    seq_value INTEGER,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE UNIQUE INDEX unq_idx_seqkeysubkey ON sequence(seq_key, seq_sub_key);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_sequence_modtime
AFTER UPDATE ON sequence
FOR EACH ROW
BEGIN
    UPDATE sequence SET modified_date = datetime('now') WHERE id = NEW.id;
END;
