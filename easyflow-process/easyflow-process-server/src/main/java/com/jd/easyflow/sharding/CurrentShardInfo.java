package com.jd.easyflow.sharding;

/**
 * @author liyuliang5
 */
public class CurrentShardInfo {


    String shard;
    boolean usingSlaveDb;
    
    String db;
    String tableSuffix = "";

    public CurrentShardInfo() {

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

    public String getShard() {
        return shard;
    }

    public void setShard(String shard) {
        this.shard = shard;
    }

    public boolean isUsingSlaveDb() {
        return usingSlaveDb;
    }

    public void setUsingSlaveDb(boolean usingSlaveDb) {
        this.usingSlaveDb = usingSlaveDb;
    }
    
}
