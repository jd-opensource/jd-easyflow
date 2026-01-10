package com.jd.easyflow.sharding;

/**
 * 
 * @author liyuliang5
 */
public class ShardingHolder {
        
    private static final ThreadLocal<CurrentShardInfo> shardingInfo = new ThreadLocal<>();
    
    public static CurrentShardInfo getCurrentShardInfo() {
        return shardingInfo.get();
    }
    
    public static void setCurrentShardInfo(CurrentShardInfo info) {
        if (info == null) {
            shardingInfo.remove();
        } else {
            shardingInfo.set(info);
        }
    }
    
    public static String getDataSourceKey() {
        CurrentShardInfo info = getCurrentShardInfo();
        return info == null ? null : info.getDb();
    }
    
    public static String getTableSuffix() {
        CurrentShardInfo info = getCurrentShardInfo();
        return info == null ? null : info.getTableSuffix();
    }
    

 }

