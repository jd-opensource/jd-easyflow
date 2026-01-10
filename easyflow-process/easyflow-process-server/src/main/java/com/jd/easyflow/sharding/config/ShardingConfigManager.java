package com.jd.easyflow.sharding.config;

import java.util.List;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 */
public interface ShardingConfigManager {

    public boolean isFixDefaultShard();

    public ShardGroupInfo getDefaultShardGroup();

    public List<ShardInfo> getShardList();
    
    public Map<String, ShardInfo> getShardMap();

    public List<DataSourceInfo> getDataSourceList();

    public Map<String, ShardGroupInfo> getShardGroupMap();
    
    public ShardingConfig getShardingConfig();

}
