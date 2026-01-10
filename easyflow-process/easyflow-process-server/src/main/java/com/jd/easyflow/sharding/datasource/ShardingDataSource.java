package com.jd.easyflow.sharding.datasource;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.datasource.lookup.AbstractRoutingDataSource;

import com.jd.easyflow.sharding.ShardingHolder;
import com.jd.easyflow.sharding.config.DataSourceInfo;
import com.jd.easyflow.sharding.config.ShardingConfigManager;
import com.jd.easyflow.sharding.datasource.impl.HikariDataSourceFactory;

/**
 * 
 * @author liyuliang5
 */
public class ShardingDataSource extends AbstractRoutingDataSource {
    
    private static final Logger logger = LoggerFactory.getLogger(ShardingDataSource.class);
    
    private boolean warnOnNoLookupKey = true;
    
    private ShardingConfigManager shardingConfigManager;
    
    private DataSourceFactory dataSourceFactory;
    
    public void setShardingTargetDataSources(Map<Object, Object> targetDataSources) {
        Map<Object, Object> configuredDataSources = getConfiguredDataSources();
        if (configuredDataSources != null) {
            if (targetDataSources == null) {
                targetDataSources = new HashMap<Object, Object>();
            }
            targetDataSources.putAll(configuredDataSources);
        }
        super.setTargetDataSources(targetDataSources);
    }
    
    protected Map<Object, Object> getConfiguredDataSources() {
        if (shardingConfigManager == null) {
            return null;
        }
        List<DataSourceInfo> dataSourceList = shardingConfigManager.getDataSourceList();
        if (dataSourceList == null) {
            return null;
        }
        if (dataSourceFactory == null) {
            dataSourceFactory = new HikariDataSourceFactory();
        }
        Map<Object, Object> dataSources = new HashMap<Object, Object>();
        for (DataSourceInfo info : dataSourceList) {
            DataSource dataSource = dataSourceFactory.createDataSource(info);
            dataSources.put(info.getId(), dataSource);
        }
        return dataSources;
    }
    
        

    @Override
    protected Object determineCurrentLookupKey() {
        String dataSourceKey = ShardingHolder.getDataSourceKey();
        if (dataSourceKey == null && warnOnNoLookupKey) {
            logger.warn("process unit dataSourceKey is null");
        }
        return dataSourceKey;
    }

    public DataSource determineTargetDataSourceInner() {
        return super.determineTargetDataSource();
    }

    public boolean isWarnOnNoLookupKey() {
        return warnOnNoLookupKey;
    }


    public void setWarnOnNoLookupKey(boolean warnOnNoLookupKey) {
        this.warnOnNoLookupKey = warnOnNoLookupKey;
    }

    public ShardingConfigManager getShardingConfigManager() {
        return shardingConfigManager;
    }

    public void setShardingConfigManager(ShardingConfigManager shardingConfigManager) {
        this.shardingConfigManager = shardingConfigManager;
    }

    public DataSourceFactory getDataSourceFactory() {
        return dataSourceFactory;
    }

    public void setDataSourceFactory(DataSourceFactory dataSourceFactory) {
        this.dataSourceFactory = dataSourceFactory;
    }
    
    
    
}

