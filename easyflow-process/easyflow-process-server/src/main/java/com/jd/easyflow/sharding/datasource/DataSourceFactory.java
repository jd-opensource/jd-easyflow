package com.jd.easyflow.sharding.datasource;

import javax.sql.DataSource;

import com.jd.easyflow.sharding.config.DataSourceInfo;

/**
 * 
 * @author liyuliang5
 */
public interface DataSourceFactory {

    DataSource createDataSource(DataSourceInfo info);
}
