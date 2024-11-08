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