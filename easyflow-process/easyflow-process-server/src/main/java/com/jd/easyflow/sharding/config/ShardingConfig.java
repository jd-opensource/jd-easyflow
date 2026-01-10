package com.jd.easyflow.sharding.config;

import java.util.List;
import java.util.Map;

/**
 * @author liyuliang5
 */
public class ShardingConfig {
    
    private boolean fixDefaultShard;

    private ShardGroupInfo defaultShardGroup;
    
    private List<ShardInfo> shardList;
    
    private List<DataSourceInfo> dataSourceList;
        
    private Map<String, ShardGroupInfo> shardGroupMap;
    
    private Map<String, ShardInfo> shardMap;


    public boolean isFixDefaultShard() {
        return fixDefaultShard;
    }

    public void setFixDefaultShard(boolean fixDefaultShard) {
        this.fixDefaultShard = fixDefaultShard;
    }

    public ShardGroupInfo getDefaultShardGroup() {
        return defaultShardGroup;
    }

    public void setDefaultShardGroup(ShardGroupInfo defaultShardGroup) {
        this.defaultShardGroup = defaultShardGroup;
    }

    public List<DataSourceInfo> getDataSourceList() {
        return dataSourceList;
    }

    public void setDataSourceList(List<DataSourceInfo> dataSourceList) {
        this.dataSourceList = dataSourceList;
    }

    public Map<String, ShardGroupInfo> getShardGroupMap() {
        return shardGroupMap;
    }

    public void setShardGroupMap(Map<String, ShardGroupInfo> shardGroupMap) {
        this.shardGroupMap = shardGroupMap;
    }

    public List<ShardInfo> getShardList() {
        return shardList;
    }

    public void setShardList(List<ShardInfo> shardList) {
        this.shardList = shardList;
    }

    public Map<String, ShardInfo> getShardMap() {
        return shardMap;
    }

    public void setShardMap(Map<String, ShardInfo> shardMap) {
        this.shardMap = shardMap;
    }

}
