package com.jd.easyflow.sharding.service;

import java.util.function.Consumer;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.sharding.CurrentShardInfo;
import com.jd.easyflow.sharding.config.ShardingConfig;
import com.jd.easyflow.sharding.config.ShardingConfigManager;

/**
 * @author liyuliang5
 */
public class ExportRequestShardComputer {
    
    private String groupKey = "_group";
    
    private String bizNoKey = "_bizNo";
    
    private ShardingService shardingService;
    
    private ShardingConfigManager shardingConfigManager;
    
    private boolean shardingEnabled;
    
    public CurrentShardInfo computeExportRequestShard(ExportRequest req, Consumer<ShardingData> consumer) {
        if (! shardingEnabled) {
            return null;
        }
        ShardingConfig shardingConfig = shardingConfigManager.getShardingConfig();
        if (shardingConfig.getShardList().size() == 1) {
            Boolean usingSlave = getExt("_usingSlave", req);
            return shardingService.buildCurrentShardInfo(shardingConfigManager.getShardList().get(0).getShard(), Boolean.TRUE.equals(usingSlave));
        }
        if (shardingConfig.isFixDefaultShard() && shardingConfig.getDefaultShardGroup().getShardList().size() == 1) {
            Boolean usingSlave = getExt("_usingSlave", req);
            return shardingService.buildCurrentShardInfo(shardingConfig.getDefaultShardGroup().getShardList().get(0), Boolean.TRUE.equals(usingSlave));
        }
        ShardingData shardingData = extractShardingData(req);
        String shard = shardingService.getShard(shardingData);
        if (shard == null && consumer != null) {
            consumer.accept(shardingData);
            shard = shardingService.getShard(shardingData);
        }
        if (shard == null) {
            return null;
        }
        CurrentShardInfo info = shardingService.buildCurrentShardInfo(shard, Boolean.TRUE.equals(shardingData.getUsingSlave()));
        return info;
    }
    

    public ShardingData extractShardingData(ExportRequest req) {
        String shard = getExt("_shard", req);
        Boolean usingSlave = getExt("_usingSlave", req);
        String processType = getExt(groupKey, req);
        String bizNo = getExt(bizNoKey, req);
        ShardingData data = new ShardingData();
        data.setShard(shard);
        data.setUsingSlave(usingSlave);
        data.setGroup(processType);
        data.setBizNo(bizNo);
        return data;
    }

    
    private static <T> T getExt(String key, ExportRequest req) {
        if (req == null) {
            return null;
        }
        if (req.getExt() == null) {
            return null;
        }
        return (T) req.getExt().get(key);
    }


    public ShardingService getShardingService() {
        return shardingService;
    }


    public void setShardingService(ShardingService shardingService) {
        this.shardingService = shardingService;
    }


    public String getGroupKey() {
        return groupKey;
    }


    public void setGroupKey(String groupKey) {
        this.groupKey = groupKey;
    }


    public String getBizNoKey() {
        return bizNoKey;
    }


    public void setBizNoKey(String bizNoKey) {
        this.bizNoKey = bizNoKey;
    }


    public ShardingConfigManager getShardingConfigManager() {
        return shardingConfigManager;
    }


    public void setShardingConfigManager(ShardingConfigManager shardingConfigManager) {
        this.shardingConfigManager = shardingConfigManager;
    }


    public boolean isShardingEnabled() {
        return shardingEnabled;
    }


    public void setShardingEnabled(boolean shardingEnabled) {
        this.shardingEnabled = shardingEnabled;
    }
    
    
}
