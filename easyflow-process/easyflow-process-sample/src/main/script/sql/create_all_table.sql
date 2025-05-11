-- -- This is mysql database schema, if you use other database, please adjust ondemand.

CREATE TABLE `form_template` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `template_code` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'template code',
  `template_name` varchar(128) DEFAULT NULL COMMENT 'template name',
  `biz_type` varchar(64)  DEFAULT NULL COMMENT 'biz type',
  `config` json DEFAULT NULL COMMENT 'config',
  `status` varchar(32)  DEFAULT NULL COMMENT 'status',
  `created_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'delete flag',
  PRIMARY KEY (`id`),
  KEY `idx_template_code` (`template_code`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='form template';

-- -- This is mysql database schema, if you use other database, please adjust ondemand.

CREATE TABLE `lock_record` (
`id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'Primary key',
`lock_key` varchar(256) DEFAULT NULL COMMENT 'lock key',
`lock_flag` char(1) DEFAULT NULL COMMENT 'lock flag',
`request_id` varchar(64) DEFAULT NULL COMMENT 'request id',
`expired_time` bigint(20) DEFAULT NULL COMMENT 'expire timestamp',
`created_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
`modified_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
`deleted` tinyint(1) NOT NULL DEFAULT '0' COMMENT 'delete flag',
PRIMARY KEY (`id`),
UNIQUE KEY `uni_idx_lock_key` (`lock_key`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='lock table';

-- -- This is mysql database schema, if you use other database, please adjust ondemand.

CREATE TABLE `sequence` (
`id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'Primary key',
`seq_key` varchar(32) DEFAULT NULL COMMENT 'sequence key',
`seq_sub_key` varchar(32) DEFAULT NULL COMMENT 'sequence subkey',
`seq_value` bigint(20) DEFAULT NULL COMMENT 'sequence value',
`created_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
`modified_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
`deleted` tinyint(1) NOT NULL DEFAULT '0' COMMENT 'delete flag',
PRIMARY KEY (`id`),
UNIQUE INDEX `unq_idx_seqkeysubkey` (`seq_key`,`seq_sub_key`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='sequence table';

-- -- This is mysql database schema, if you use other database, please adjust ondemand.

-- process definition
CREATE TABLE `process_definition` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `def_id` varchar(128) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition ID',
  `def_version` int(11) DEFAULT NULL COMMENT 'definition version',
  `name` varchar(128) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition name',
  `format` varchar(32) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition format',
  `biz_type` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition biz type',
  `category` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition category',
  `content` mediumtext COLLATE utf8mb4_bin COMMENT 'definition content',
  `json_content` mediumtext COLLATE utf8mb4_bin COMMENT 'json definition content',
  `latest` tinyint(4) DEFAULT NULL COMMENT 'latest flag, 1 is latest',
  `def_source` varchar(32) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition source',
  `ext_data` json DEFAULT NULL COMMENT 'extension data',
  `created_by` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition created by',
  `modified_by` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'definition modified by',
  `created_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'delete flag',
  PRIMARY KEY (`id`),
  INDEX `idx_defId_defVersion` (`def_id`,`def_version`),
  INDEX `idx_category` (`category`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='process definition table';
-- process instance
CREATE TABLE `process_instance` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `instance_no` VARCHAR(64) NULL COMMENT 'instance no',
  `instance_name` VARCHAR(128) NULL COMMENT 'instance name',
  `process_type` VARCHAR(32) NULL COMMENT 'process type',
  `biz_no` VARCHAR(128) NULL COMMENT 'biz no',
  `product_code` VARCHAR(64) NULL COMMENT 'product code',
  `creator` VARCHAR(64) NULL COMMENT 'instance creator',
  `process_def_id` VARCHAR(128) NULL COMMENT 'definition ID',
  `start_time` datetime NULL COMMENT 'instance start time',
  `end_time` datetime NULL COMMENT 'instance end time',
  `status` VARCHAR(32) NULL COMMENT 'instance status',
  `parent_instance_no` VARCHAR(64) NULL COMMENT 'parent instance no',
  `parent_node_instance_no` VARCHAR(64) NULL COMMENT 'parent node instance no',
  `current_node_ids` VARCHAR(512) NULL COMMENT 'current node id list',
  `biz_status` VARCHAR(64) NULL COMMENT 'instance biz status',
  `biz_data` TEXT NULL COMMENT 'instance biz data',
  `key_field` VARCHAR(128) NULL COMMENT 'key field',
  `key_field2` VARCHAR(128) NULL COMMENT 'key field2',
  `vars` JSON NULL COMMENT 'instance variables',
  `ext_data` JSON NULL COMMENT 'extension data',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'deleted flag',
  PRIMARY KEY (`id`),
  INDEX `idx_instanceNo` (`instance_no`),
  INDEX `idx_bizNo_processType` (`biz_no`, process_type),
  INDEX `idx_startTime` (`start_time`),
  INDEX `idx_creator` (`creator`),
  INDEX `idx_productCode` (`product_code`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET= utf8mb4
COLLATE = utf8mb4_bin
COMMENT = 'process instance table';

-- process node instance
CREATE TABLE `process_node_instance` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `node_instance_no` VARCHAR(64) NULL COMMENT 'node instance no',
  `process_instance_no` VARCHAR(64) NULL COMMENT 'process instance no',
  `process_def_id` VARCHAR(128) NULL COMMENT 'definition ID',
  `node_id` VARCHAR(128) NULL COMMENT 'node ID',
  `start_time` DATETIME NULL COMMENT 'node instance start time',
  `end_time` DATETIME NULL COMMENT 'node instance end time',
  `status` VARCHAR(32) NULL COMMENT 'node instance status',
  `product_code` VARCHAR(64) NULL COMMENT 'product code',
  `executors` VARCHAR(1024) NULL COMMENT 'node executor',
  `previous_node_instances` VARCHAR(512) NULL COMMENT 'previous node instances',  
  `next_node_instances` VARCHAR(512) NULL COMMENT 'next node instances',
  `vars` JSON NULL COMMENT 'node instance variables',
  `ext_data` JSON NULL COMMENT 'extension data',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'deleted flag',
  PRIMARY KEY (`id`),
  INDEX `idx_nodeInstanceNo` (`node_instance_no`),
  INDEX `idx_processInstanceNo` (`process_instance_no`),
  INDEX `idx_startTime` (`start_time`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = 'process node instance table';

-- process node execution
CREATE TABLE `process_node_execution` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `node_execution_no` VARCHAR(64) NULL COMMENT 'node execution no',
  `node_instance_no` VARCHAR(64) NULL COMMENT 'node instance no',
  `event_id` VARCHAR(128) NULL COMMENT 'event ID',
  `process_def_id` VARCHAR(128) NULL COMMENT 'definition ID',
  `node_id` VARCHAR(128) NULL COMMENT 'node ID',
  `start_time` DATETIME NULL COMMENT 'start time',
  `end_time` DATETIME NULL COMMENT 'end time',
  `status` VARCHAR(32) NULL COMMENT 'status',
  `product_code` VARCHAR(64) NULL COMMENT 'product code',
  `executor` VARCHAR(64) NULL COMMENT 'executor',
  `next_node_instances` VARCHAR(512) NULL COMMENT 'next node instances',
  `ext_data` JSON NULL COMMENT 'extension data',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'delete flag',
  PRIMARY KEY (`id`),
  INDEX `idx_executionNo` (`node_execution_no`),
  INDEX `idx_nodeInstanceNo` (`node_instance_no`),
  INDEX `idx_startTime` (`start_time`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = 'process node execution table';

-- process task
CREATE TABLE `process_task` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `task_no` VARCHAR(64)  NULL COMMENT 'task no',
  `process_type` VARCHAR(32) NULL COMMENT 'process type',
  `biz_no` VARCHAR(128) NULL COMMENT 'biz no',
  `task_biz_code` VARCHAR(64)  NULL COMMENT 'task biz code',
  `task_biz_name` VARCHAR(128)  NULL COMMENT 'task biz name',
  `task_type` VARCHAR(64)  NULL COMMENT 'task type',
  `process_instance_no` VARCHAR(64) NULL COMMENT 'process instance no',  
  `node_instance_no` VARCHAR(64) NULL COMMENT 'node instance no',
  `node_execution_no` VARCHAR(64) NULL COMMENT 'node execution no',
  `assign_type`  VARCHAR(64) NULL COMMENT 'assign type',
  `assign_info`  JSON NULL COMMENT 'assign info',
  `assign_time`  DATETIME NULL COMMENT 'assign time',
  `executor`  VARCHAR(64) NULL COMMENT 'executor',
  `execute_biz_result` VARCHAR(64) COMMENT 'execute biz result',
  `execute_biz_data` TEXT NULL COMMENT 'execute biz data',
  `execute_time` DATETIME NULL COMMENT 'execute time',
  `product_code`  VARCHAR(64) NULL COMMENT 'product code',
  `creator` VARCHAR(64) NULL COMMENT 'creator',
  `status` VARCHAR(32) NULL COMMENT 'task status',
  `ext_data` JSON NULL COMMENT 'extension data',
  `process_instance_key_field` VARCHAR(128) NULL COMMENT 'process instance key field',
  `process_instance_key_field2` VARCHAR(128) NULL COMMENT 'process instance key field2',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'delete flag',
  PRIMARY KEY (`id`),
  INDEX `idx_taskNo` (`task_no`),
  INDEX `idx_bizNoProcessType` (`biz_no`, process_type),
  INDEX `idx_executeTime` (`execute_time`),
  INDEX `idx_assignTime` (`assign_time`),
  INDEX `idx_executor` (`executor`),
  INDEX `idx_processInstanceKeyField` (`process_instance_key_field`),
  INDEX `idx_processInstanceKeyField2` (`process_instance_key_field2`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = 'process task table';

-- process task assign
CREATE TABLE `process_task_assign` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `assign_no` VARCHAR(64)  NULL COMMENT 'assign no',
  `task_no` VARCHAR(64) NULL COMMENT 'task no',
  `assign_type` VARCHAR(32) NULL COMMENT 'assign type',
  `assign_group` VARCHAR(64) NULL COMMENT 'assign group',
  `assign_group2` VARCHAR(64) NULL COMMENT 'assign group2',
  `assign_user` VARCHAR(32) NULL COMMENT 'assign user',
  `operation`   VARCHAR(32) NULL COMMENT 'operation',
  `status` VARCHAR(32) NULL COMMENT 'status',
  `assign_time`  DATETIME NULL COMMENT 'assign time',
  `product_code`  VARCHAR(64) NULL COMMENT 'product code',
  `ext_data` JSON NULL COMMENT 'extension data',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'delete flag',
  PRIMARY KEY (`id`),
  INDEX `idx_assignNo` (`assign_no`), 
  INDEX `idx_taskNo` (`task_no`),
  INDEX `idx_assignGroup` (`assign_group`),
  INDEX `idx_assignGroup2` (`assign_group2`),
  INDEX `idx_assignUser` (`assign_user`),
  INDEX `idx_assignTime` (`assign_time`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = 'process task assign table';

-- process task event.
CREATE TABLE `process_task_event` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT 'primary key',
  `event_no` VARCHAR(64)  NULL COMMENT 'event no',
  `task_no` VARCHAR(64) NULL COMMENT 'task no',
  `event_type` VARCHAR(32) NULL COMMENT 'event type',
  `event_user` VARCHAR(64) NULL COMMENT 'event user',
  `event_time` DATETIME NULL COMMENT 'event time',
  `event_biz_result` VARCHAR(64) COMMENT 'event biz result',
  `event_biz_data` TEXT NULL COMMENT 'event biz data',
  `instance_biz_status` VARCHAR(64) COMMENT 'instance biz status',
  `instance_biz_data` TEXT NULL COMMENT 'instance biz data',  
  `product_code`  VARCHAR(64) NULL COMMENT 'product code',
  `ext_data` JSON NULL COMMENT 'extension data',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'created date',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'modified date',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'delete flag',
  PRIMARY KEY (`id`),
  INDEX `idx_eventNo` (`event_no`), 
  INDEX `idx_taskNo` (`task_no`),
  INDEX `idx_eventUser` (`event_user`),
  INDEX `idx_eventTime` (`event_time`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = 'process task event table';