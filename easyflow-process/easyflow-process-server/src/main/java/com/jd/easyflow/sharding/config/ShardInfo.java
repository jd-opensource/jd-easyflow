package com.jd.easyflow.sharding.config;

/**
 * 
 * @author liyuliang5
 */
public class ShardInfo {
    
    String shard;
    
    String db;
    String slaveDb;
    String tableSuffix = "";
    
    boolean master = true;

    public ShardInfo() {

    }

    public String getShard() {
        return shard;
    }

    public void setShard(String shard) {
        this.shard = shard;
    }

    public String getDb() {
        return db;
    }

    public void setDb(String db) {
        this.db = db;
    }

    public String getTableSuffix() {
        return tableSuffix;
    }

    public void setTableSuffix(String tableSuffix) {
        this.tableSuffix = tableSuffix;
    }

    public String getSlaveDb() {
        return slaveDb;
    }

    public void setSlaveDb(String slaveDb) {
        this.slaveDb = slaveDb;
    }

    public boolean isMaster() {
        return master;
    }

    public void setMaster(boolean master) {
        this.master = master;
    }
    
}
