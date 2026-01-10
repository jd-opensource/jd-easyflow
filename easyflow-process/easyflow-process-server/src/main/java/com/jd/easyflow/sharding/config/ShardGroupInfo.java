package com.jd.easyflow.sharding.config;

import java.util.List;

/**
 * @author liyuliang5
 */
public class ShardGroupInfo {

    private List<String> shardList;
    
    private Boolean doubleWrite;
    
    private Boolean ignoreSlaveError;
    
    private List<String> slaveShardList;

    public List<String> getShardList() {
        return shardList;
    }

    public void setShardList(List<String> shardList) {
        this.shardList = shardList;
    }

    public Boolean getDoubleWrite() {
        return doubleWrite;
    }

    public void setDoubleWrite(Boolean doubleWrite) {
        this.doubleWrite = doubleWrite;
    }

    public Boolean getIgnoreSlaveError() {
        return ignoreSlaveError;
    }

    public void setIgnoreSlaveError(Boolean ignoreSlaveError) {
        this.ignoreSlaveError = ignoreSlaveError;
    }

    public List<String> getSlaveShardList() {
        return slaveShardList;
    }

    public void setSlaveShardList(List<String> slaveShardList) {
        this.slaveShardList = slaveShardList;
    }
    
    
    
}
