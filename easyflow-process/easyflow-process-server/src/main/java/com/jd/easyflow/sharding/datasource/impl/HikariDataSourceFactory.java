package com.jd.easyflow.sharding.datasource.impl;

import javax.sql.DataSource;

import com.jd.easyflow.sharding.config.DataSourceInfo;
import com.jd.easyflow.sharding.datasource.DataSourceFactory;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

/**
 * @author liyuliang5
 */
public class HikariDataSourceFactory implements DataSourceFactory {

    @Override
    public DataSource createDataSource(DataSourceInfo info) {
        HikariConfig config = new HikariConfig(info.getProperties());
        HikariDataSource dataSource = new HikariDataSource(config);
        return dataSource;
    }
}
