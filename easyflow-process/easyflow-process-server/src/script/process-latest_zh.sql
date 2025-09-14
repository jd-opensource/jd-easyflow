-- 流程定义表，流程定义持久化到数据库场景中使用
CREATE TABLE `process_definition` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `def_id` varchar(128) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '流程定义ID',
  `def_version` int(11) DEFAULT NULL COMMENT '流程版本号（仅查询使用）',
  `name` varchar(128) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '流程定义名称',
  `format` varchar(32) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '流程定义格式',
  `biz_type` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '流程业务类型',
  `category` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '业务类型',
  `content` mediumtext COLLATE utf8mb4_bin COMMENT '流程定义内容',
  `json_content` mediumtext COLLATE utf8mb4_bin COMMENT 'json格式内容',
  `latest` tinyint(4) DEFAULT NULL COMMENT '最新版本标识(最新标识为1)',
  `def_source` varchar(32) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '来源',
  `ext_data` json DEFAULT NULL COMMENT '扩展字段',
  `created_by` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `modified_by` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '修改者',
  `created_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modified_date` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT '删除标识',
  PRIMARY KEY (`id`),
  INDEX `idx_def_id_def_version` (`def_id`,`def_version`),
  INDEX `idx_category` (`category`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='流程定义表';
-- 流程实例表，流程实例持久化功能使用
CREATE TABLE `process_instance` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT '主键',
  `instance_no` VARCHAR(64) NULL COMMENT '实例编号',
  `instance_name` VARCHAR(128) NULL COMMENT '实例名称',
  `process_type` VARCHAR(32) NULL COMMENT '流程类型',
  `biz_no` VARCHAR(128) NULL COMMENT '业务编号',
  `product_code` VARCHAR(64) NULL COMMENT '产品编码',
  `creator` VARCHAR(64) NULL COMMENT '流程创建用户编号',
  `process_def_id` VARCHAR(128) NULL COMMENT '流程ID',
  `start_time` datetime NULL COMMENT '流程开始时间',
  `end_time` datetime NULL COMMENT '流程结束时间',
  `status` VARCHAR(32) NULL COMMENT '流程状态',
  `parent_instance_no` VARCHAR(64) NULL COMMENT '父实例编号',
  `parent_node_instance_no` VARCHAR(64) NULL COMMENT '父节点实例编号',
  `current_node_ids` VARCHAR(512) NULL COMMENT '当前节点列表',
  `biz_status` VARCHAR(64) NULL COMMENT '流程实例业务状态',
  `biz_data` TEXT NULL COMMENT '流程实例业务数据',
  `vars` JSON NULL COMMENT '流程实例变量',
  `ext_data` JSON NULL COMMENT '扩展字段',
  `key_field` VARCHAR(128) NULL COMMENT '关键字',  
  `key_field2` VARCHAR(128) NULL COMMENT '关键字2',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT '删除标识',
  PRIMARY KEY (`id`),
  INDEX `idx_instanceNo` (`instance_no`),
  INDEX `idx_bizNo_processType` (`biz_no`, process_type),
  INDEX `idx_startTime` (`start_time`),
  INDEX `idx_creator` (`creator`),
  INDEX `idx_product_code` (`product_code`),
  INDEX `idx_keyField` (`key_field`),
  INDEX `idx_parentInstanceNo`(`parent_instance_no`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET= utf8mb4
COLLATE = utf8mb4_bin
COMMENT = '流程实例表';

-- 节点实例表，流程实例持久化功能使用
CREATE TABLE `process_node_instance` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT '主键',
  `node_instance_no` VARCHAR(64) NULL COMMENT '节点实例编号',
  `process_instance_no` VARCHAR(64) NULL COMMENT '流程实例编号',
  `process_def_id` VARCHAR(128) NULL COMMENT '流程定义ID',
  `node_id` VARCHAR(128) NULL COMMENT '节点ID',
  `start_time` DATETIME NULL COMMENT '节点开始时间',
  `end_time` DATETIME NULL COMMENT '节点结束时间',
  `status` VARCHAR(32) NULL COMMENT '节点状态',
  `product_code` VARCHAR(64) NULL COMMENT '产品编码',
  `executors` VARCHAR(1024) NULL COMMENT '待办用户列表',
  `previous_node_instances` VARCHAR(512) NULL COMMENT '前置节点实例',  
  `next_node_instances` VARCHAR(512) NULL COMMENT '后续节点实例',
  `vars` JSON NULL COMMENT '节点实例变量',
  `ext_data` JSON NULL COMMENT '扩展字段',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT '删除标识',
  PRIMARY KEY (`id`),
  INDEX `idx_nodeInstanceNo` (`node_instance_no`),
  INDEX `idx_processInstanceNo` (`process_instance_no`),
  INDEX `idx_startTime` (`start_time`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = '节点实例表';

-- 流程实例执行表，流程实例持久化功能使用
CREATE TABLE `process_node_execution` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT '主键',
  `node_execution_no` VARCHAR(64) NULL COMMENT '节点执行编号',
  `node_instance_no` VARCHAR(64) NULL COMMENT '节点实例编号',
  `event_id` VARCHAR(128) NULL COMMENT '事件ID',
  `process_def_id` VARCHAR(128) NULL COMMENT '流程定义ID',
  `node_id` VARCHAR(128) NULL COMMENT '节点ID',
  `start_time` DATETIME NULL COMMENT '开始时间',
  `end_time` DATETIME NULL COMMENT '结束时间',
  `status` VARCHAR(32) NULL COMMENT '执行状态',
  `product_code` VARCHAR(64) NULL COMMENT '产品编码',
  `executor` VARCHAR(64) NULL COMMENT '执行用户编号',
  `next_node_instances` VARCHAR(512) NULL COMMENT '后续节点实例',
  `ext_data` JSON NULL COMMENT '扩展字段',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT '删除标识',
  PRIMARY KEY (`id`),
  INDEX `idx_executionNo` (`node_execution_no`),
  INDEX `idx_nodeInstanceNo` (`node_instance_no`),
  INDEX `idx_startTime` (`start_time`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = '节点实例执行表';

-- 任务表
CREATE TABLE `process_task` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT '主键',
  `task_no` VARCHAR(64)  NULL COMMENT '流程任务编号',
  `process_type` VARCHAR(32) NULL COMMENT '流程类型',
  `biz_no` VARCHAR(128) NULL COMMENT '业务编号',
  `task_biz_code` VARCHAR(64)  NULL COMMENT '流程任务业务编码',
  `task_biz_name` VARCHAR(128)  NULL COMMENT '流程任务业务名称，冗余字段',
  `task_type` VARCHAR(64)  NULL COMMENT '流程任务类型,人工任务、消息任务等',
  `process_instance_no` VARCHAR(64) NULL COMMENT '流程实例编号',  
  `node_instance_no` VARCHAR(64) NULL COMMENT '节点实例编号',
  `node_execution_no` VARCHAR(64) NULL COMMENT '节点执行编号',
  `assign_type`  VARCHAR(64) NULL COMMENT '任务分配类型',
  `assign_info`  JSON NULL COMMENT '任务分配信息',
  `assign_time`  DATETIME NULL COMMENT '分配时间',
  `executor`  VARCHAR(64) NULL COMMENT '执行用户号',
  `execute_biz_result` VARCHAR(64) COMMENT '执行的业务结果',
  `execute_biz_data` TEXT NULL COMMENT '执行的业务数据',
  `execute_time` DATETIME NULL COMMENT '执行时间',
  `product_code`  VARCHAR(64) NULL COMMENT '产品编码',
  `creator` VARCHAR(64) NULL COMMENT '流程创建用户编号',
  `status` VARCHAR(32) NULL COMMENT '任务状态',
  `ext_data` JSON NULL COMMENT '扩展字段',
  `process_instance_key_field` VARCHAR(128) NULL COMMENT '流程关键字',  
  `process_instance_key_field2` VARCHAR(128) NULL COMMENT '流程关键字2',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT '删除标识',
  PRIMARY KEY (`id`),
  INDEX `idx_taskNo` (`task_no`),
  INDEX `idx_bizNoProcessType` (`biz_no`, process_type),
  INDEX `idx_processInstanceNo` (`process_instance_no`),
  INDEX `idx_nodeInstanceNo` (`node_instance_no`),  
  INDEX `idx_executeTime` (`execute_time`),
  INDEX `idx_assignTime` (`assign_time`),
  INDEX `idx_executor` (`executor`),
  INDEX `idx_processInstanceKeyField` (`process_instance_key_field`),
  INDEX `idx_processInstanceKeyField2` (`process_instance_key_field2`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = '任务表';

-- 任务分配表
CREATE TABLE `process_task_assign` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT '主键',
  `assign_no` VARCHAR(64)  NULL COMMENT '任务分配编号',
  `task_no` VARCHAR(64) NULL COMMENT '流程任务编号',
  `assign_type` VARCHAR(32) NULL COMMENT '分配类型',
  `assign_group` VARCHAR(64) NULL COMMENT '分配组',
  `assign_group2` VARCHAR(64) NULL COMMENT '分配组2',
  `assign_user` VARCHAR(32) NULL COMMENT '分配用户',
  `operation`   VARCHAR(32) NULL COMMENT '可执行的操作',
  `status` VARCHAR(32) NULL COMMENT '任务状态',
  `assign_time`  DATETIME NULL COMMENT '分配时间',
  `product_code`  VARCHAR(64) NULL COMMENT '产品编码',
  `ext_data` JSON NULL COMMENT '扩展字段',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT '删除标识',
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
COMMENT = '任务分配表';

-- 任务事件表
CREATE TABLE `process_task_event` (
  `id` BIGINT NOT NULL AUTO_INCREMENT COMMENT '主键',
  `event_no` VARCHAR(64)  NULL COMMENT '事件编号',
  `task_no` VARCHAR(64) NULL COMMENT '流程任务编号',
  `event_type` VARCHAR(32) NULL COMMENT '事件类型,CREATE:创建, SUBMIT:提交',
  `event_user` VARCHAR(64) NULL COMMENT '事件执行人',
  `event_time` DATETIME NULL COMMENT '事件时间',
  `event_biz_result` VARCHAR(64) COMMENT '事件业务结果',
  `event_biz_data` TEXT NULL COMMENT '事件业务数据',
  `instance_biz_status` VARCHAR(64) COMMENT '事件业务状态',
  `instance_biz_data` TEXT NULL COMMENT '事件业务数据',  
  `product_code`  VARCHAR(64) NULL COMMENT '产品编码',
  `ext_data` JSON NULL COMMENT '扩展字段',
  `created_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modified_date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
  `deleted` TINYINT(1) NOT NULL DEFAULT 0 COMMENT '删除标识',
  PRIMARY KEY (`id`),
  INDEX `idx_eventNo` (`event_no`), 
  INDEX `idx_taskNo` (`task_no`),
  INDEX `idx_eventUser` (`event_user`),
  INDEX `idx_eventTime` (`event_time`))
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARSET = utf8mb4
COLLATE = utf8mb4_bin
COMMENT = '任务事件表';