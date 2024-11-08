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