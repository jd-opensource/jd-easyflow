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