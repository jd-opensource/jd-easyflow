package com.jd.easyflow.sharding.service;

import java.io.Serializable;

public class ShardingComputeResult implements Serializable {

    private String shard;

    private String db;

    private String tableSuffix;

    private String slaveShard;

    private String slaveDb;

    private String slaveTableSuffix;

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

    public String getSlaveTableSuffix() {
        return slaveTableSuffix;
    }

    public void setSlaveTableSuffix(String slaveTableSuffix) {
        this.slaveTableSuffix = slaveTableSuffix;
    }

    public String getSlaveShard() {
        return slaveShard;
    }

    public void setSlaveShard(String slaveShard) {
        this.slaveShard = slaveShard;
    }

    @Override
    public String toString() {
        return "ShardingInfoVO [shard=" + shard + ", db=" + db + ", tableSuffix=" + tableSuffix + ", slaveShard="
                + slaveShard + ", slaveDb=" + slaveDb + ", slaveTableSuffix=" + slaveTableSuffix + "]";
    }
}
