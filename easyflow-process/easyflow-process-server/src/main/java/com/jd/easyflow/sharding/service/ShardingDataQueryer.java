package com.jd.easyflow.sharding.service;

/**
 * @author liyuliang5
 */
public abstract class ShardingDataQueryer {
    
    protected ShardingService shardingService;

    public ShardingService getShardingService() {
        return shardingService;
    }

    public void setShardingService(ShardingService shardingService) {
        this.shardingService = shardingService;
    }
    
    

}
