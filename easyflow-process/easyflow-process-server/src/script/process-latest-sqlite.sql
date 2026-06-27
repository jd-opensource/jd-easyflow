-- SQLite database schema for process module

-- process definition
CREATE TABLE process_definition (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    def_id TEXT,
    def_version INTEGER,
    name TEXT,
    format TEXT,
    biz_type TEXT,
    category TEXT,
    content TEXT,
    json_content TEXT,
    latest INTEGER,
    def_source TEXT,
    ext_data TEXT,
    created_by TEXT,
    modified_by TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pd_defId_defVersion ON process_definition(def_id, def_version);
CREATE INDEX idx_pd_category ON process_definition(category);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_process_definition_modtime
AFTER UPDATE ON process_definition
FOR EACH ROW
BEGIN
    UPDATE process_definition SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process instance
CREATE TABLE process_instance (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    instance_no TEXT,
    instance_name TEXT,
    process_type TEXT,
    biz_no TEXT,
    product_code TEXT,
    creator TEXT,
    process_def_id TEXT,
    start_time TEXT,
    end_time TEXT,
    status TEXT,
    parent_instance_no TEXT,
    parent_node_instance_no TEXT,
    current_node_ids TEXT,
    biz_status TEXT,
    biz_data TEXT,
    key_field TEXT,
    key_field2 TEXT,
    vars TEXT,
    ext_data TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pi_instanceNo ON process_instance(instance_no);
CREATE INDEX idx_pi_bizNo_processType ON process_instance(biz_no, process_type);
CREATE INDEX idx_pi_startTime ON process_instance(start_time);
CREATE INDEX idx_pi_creator ON process_instance(creator);
CREATE INDEX idx_pi_productCode ON process_instance(product_code);
CREATE INDEX idx_pi_keyField ON process_instance(key_field);
CREATE INDEX idx_pi_parentInstanceNo ON process_instance(parent_instance_no);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_process_instance_modtime
AFTER UPDATE ON process_instance
FOR EACH ROW
BEGIN
    UPDATE process_instance SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process node instance
CREATE TABLE process_node_instance (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    node_instance_no TEXT,
    process_instance_no TEXT,
    process_def_id TEXT,
    node_id TEXT,
    start_time TEXT,
    end_time TEXT,
    status TEXT,
    product_code TEXT,
    executors TEXT,
    previous_node_instances TEXT,
    next_node_instances TEXT,
    vars TEXT,
    ext_data TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pni_nodeInstanceNo ON process_node_instance(node_instance_no);
CREATE INDEX idx_pni_processInstanceNo ON process_node_instance(process_instance_no);
CREATE INDEX idx_pni_startTime ON process_node_instance(start_time);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_process_node_instance_modtime
AFTER UPDATE ON process_node_instance
FOR EACH ROW
BEGIN
    UPDATE process_node_instance SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process node execution
CREATE TABLE process_node_execution (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    node_execution_no TEXT,
    node_instance_no TEXT,
    event_id TEXT,
    process_def_id TEXT,
    node_id TEXT,
    start_time TEXT,
    end_time TEXT,
    status TEXT,
    product_code TEXT,
    executor TEXT,
    next_node_instances TEXT,
    ext_data TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pne_executionNo ON process_node_execution(node_execution_no);
CREATE INDEX idx_pne_nodeInstanceNo ON process_node_execution(node_instance_no);
CREATE INDEX idx_pne_startTime ON process_node_execution(start_time);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_process_node_execution_modtime
AFTER UPDATE ON process_node_execution
FOR EACH ROW
BEGIN
    UPDATE process_node_execution SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process task
CREATE TABLE process_task (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    task_no TEXT,
    process_type TEXT,
    biz_no TEXT,
    task_biz_code TEXT,
    task_biz_name TEXT,
    task_type TEXT,
    process_instance_no TEXT,
    node_instance_no TEXT,
    node_execution_no TEXT,
    assign_type TEXT,
    assign_info TEXT,
    assign_time TEXT,
    executor TEXT,
    execute_biz_result TEXT,
    execute_biz_data TEXT,
    execute_time TEXT,
    product_code TEXT,
    creator TEXT,
    status TEXT,
    ext_data TEXT,
    process_instance_key_field TEXT,
    process_instance_key_field2 TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pt_taskNo ON process_task(task_no);
CREATE INDEX idx_pt_bizNoProcessType ON process_task(biz_no, process_type);
CREATE INDEX idx_pt_processInstanceNo ON process_task(process_instance_no);
CREATE INDEX idx_pt_nodeInstanceNo ON process_task(node_instance_no);
CREATE INDEX idx_pt_executeTime ON process_task(execute_time);
CREATE INDEX idx_pt_assignTime ON process_task(assign_time);
CREATE INDEX idx_pt_executor ON process_task(executor);
CREATE INDEX idx_pt_processInstanceKeyField ON process_task(process_instance_key_field);
CREATE INDEX idx_pt_processInstanceKeyField2 ON process_task(process_instance_key_field2);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_process_task_modtime
AFTER UPDATE ON process_task
FOR EACH ROW
BEGIN
    UPDATE process_task SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process task assign
CREATE TABLE process_task_assign (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    assign_no TEXT,
    task_no TEXT,
    assign_type TEXT,
    assign_group TEXT,
    assign_group2 TEXT,
    assign_user TEXT,
    operation TEXT,
    status TEXT,
    assign_time TEXT,
    product_code TEXT,
    ext_data TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pta_assignNo ON process_task_assign(assign_no);
CREATE INDEX idx_pta_taskNo ON process_task_assign(task_no);
CREATE INDEX idx_pta_assignGroup ON process_task_assign(assign_group);
CREATE INDEX idx_pta_assignGroup2 ON process_task_assign(assign_group2);
CREATE INDEX idx_pta_assignUser ON process_task_assign(assign_user);
CREATE INDEX idx_pta_assignTime ON process_task_assign(assign_time);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_process_task_assign_modtime
AFTER UPDATE ON process_task_assign
FOR EACH ROW
BEGIN
    UPDATE process_task_assign SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process task event
CREATE TABLE process_task_event (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    event_no TEXT,
    task_no TEXT,
    event_type TEXT,
    event_user TEXT,
    event_time TEXT,
    event_biz_result TEXT,
    event_biz_data TEXT,
    instance_biz_status TEXT,
    instance_biz_data TEXT,
    product_code TEXT,
    ext_data TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pte_eventNo ON process_task_event(event_no);
CREATE INDEX idx_pte_taskNo ON process_task_event(task_no);
CREATE INDEX idx_pte_eventUser ON process_task_event(event_user);
CREATE INDEX idx_pte_eventTime ON process_task_event(event_time);

-- Trigger: auto-update modified_date on row update
CREATE TRIGGER update_process_task_event_modtime
AFTER UPDATE ON process_task_event
FOR EACH ROW
BEGIN
    UPDATE process_task_event SET modified_date = datetime('now') WHERE id = NEW.id;
END;
