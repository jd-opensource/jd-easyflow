package com.jd.easyflow.sharding.config.impl;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.sharding.config.DataSourceInfo;
import com.jd.easyflow.sharding.config.ShardGroupInfo;
import com.jd.easyflow.sharding.config.ShardInfo;
import com.jd.easyflow.sharding.config.ShardingConfig;
import com.jd.easyflow.sharding.config.ShardingConfigManager;

/**
 * @author liyuliang5
 */
public  class ShardingConfigManagerImpl implements ShardingConfigManager {
    
    protected volatile ShardingConfig shardingConfig;

    @Override
    public boolean isFixDefaultShard() {
        return shardingConfig.isFixDefaultShard();
    }

    @Override
    public ShardGroupInfo getDefaultShardGroup() {
        return shardingConfig.getDefaultShardGroup();
    }

    @Override
    public List<ShardInfo> getShardList() {
        return shardingConfig.getShardList();
    }
    
    @Override
    public Map<String, ShardInfo> getShardMap() {
        return shardingConfig.getShardMap();
    }

    @Override
    public List<DataSourceInfo> getDataSourceList() {
        return shardingConfig.getDataSourceList();
    }

    @Override
    public Map<String, ShardGroupInfo> getShardGroupMap() {
        return shardingConfig.getShardGroupMap();
    }

    @Override
    public ShardingConfig getShardingConfig() {
        return shardingConfig;
    }

    public void setShardingConfig(ShardingConfig shardingConfig) {
        this.shardingConfig = shardingConfig;
    }
    



}
