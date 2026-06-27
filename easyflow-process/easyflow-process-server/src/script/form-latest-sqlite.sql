-- SQLite database schema for form module

CREATE TABLE form_template (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    template_code TEXT,
    template_name TEXT,
    biz_type TEXT,
    config TEXT,
    status TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_template_code ON form_template(template_code);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_form_template_modtime
AFTER UPDATE ON form_template
FOR EACH ROW
BEGIN
    UPDATE form_template SET modified_date = datetime('now') WHERE id = NEW.id;
END;
