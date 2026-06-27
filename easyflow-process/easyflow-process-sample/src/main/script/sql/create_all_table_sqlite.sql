-- SQLite database schema - all tables combined
-- Compatible with easyflow sample application

-- form template
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

CREATE INDEX idx_ft_template_code ON form_template(template_code);

CREATE TRIGGER update_form_template_modtime
AFTER UPDATE ON form_template
FOR EACH ROW
BEGIN
    UPDATE form_template SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- lock record
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

CREATE UNIQUE INDEX idx_lr_uni_lock_key ON lock_record(lock_key);

CREATE TRIGGER update_lock_record_modtime
AFTER UPDATE ON lock_record
FOR EACH ROW
BEGIN
    UPDATE lock_record SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- sequence
CREATE TABLE sequence (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    seq_key TEXT,
    seq_sub_key TEXT,
    seq_value INTEGER,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE UNIQUE INDEX idx_seq_unq_seqkeysubkey ON sequence(seq_key, seq_sub_key);

CREATE TRIGGER update_sequence_modtime
AFTER UPDATE ON sequence
FOR EACH ROW
BEGIN
    UPDATE sequence SET modified_date = datetime('now') WHERE id = NEW.id;
END;

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

CREATE TRIGGER update_process_task_event_modtime
AFTER UPDATE ON process_task_event
FOR EACH ROW
BEGIN
    UPDATE process_task_event SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process unit instance
CREATE TABLE process_unit_instance (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    instance_no TEXT,
    biz_no TEXT,
    parent_no TEXT,
    process_unit_code TEXT,
    product_code TEXT,
    result TEXT,
    request_content TEXT,
    response_content TEXT,
    auto_run_flag INTEGER,
    auto_run_times INTEGER,
    next_auto_run_time TEXT,
    vars TEXT,
    ext_data TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE UNIQUE INDEX idx_pui_instance_no ON process_unit_instance(instance_no);
CREATE UNIQUE INDEX idx_pui_bizNo_unitCode ON process_unit_instance(biz_no, process_unit_code);
CREATE INDEX idx_pui_autorun ON process_unit_instance(auto_run_flag, next_auto_run_time);
CREATE INDEX idx_pui_createdDate ON process_unit_instance(created_date);

CREATE TRIGGER update_process_unit_instance_modtime
AFTER UPDATE ON process_unit_instance
FOR EACH ROW
BEGIN
    UPDATE process_unit_instance SET modified_date = datetime('now') WHERE id = NEW.id;
END;

-- process unit execution
CREATE TABLE process_unit_execution (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    execution_no TEXT,
    request_no TEXT,
    parent_no TEXT,
    process_unit_code TEXT,
    biz_no TEXT,
    instance_no TEXT,
    product_code TEXT,
    result TEXT,
    request_content TEXT,
    response_content TEXT,
    request_time TEXT,
    response_time TEXT,
    elaspe_time INTEGER,
    exec_type TEXT,
    ext_data TEXT,
    created_date TEXT NOT NULL DEFAULT (datetime('now')),
    modified_date TEXT NOT NULL DEFAULT (datetime('now')),
    deleted INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_pue_execution_no ON process_unit_execution(execution_no);
CREATE INDEX idx_pue_instance_no ON process_unit_execution(instance_no);
CREATE INDEX idx_pue_request_time ON process_unit_execution(request_time);
CREATE INDEX idx_pue_bizNo_unitCode ON process_unit_execution(biz_no, process_unit_code);

CREATE TRIGGER update_process_unit_execution_modtime
AFTER UPDATE ON process_unit_execution
FOR EACH ROW
BEGIN
    UPDATE process_unit_execution SET modified_date = datetime('now') WHERE id = NEW.id;
END;
