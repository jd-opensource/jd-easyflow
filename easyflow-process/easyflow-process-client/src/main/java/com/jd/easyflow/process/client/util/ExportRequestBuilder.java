package com.jd.easyflow.process.client.util;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.process.adapter.export.constant.ShardingConstants;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * @author liyuliang5
 */
public class ExportRequestBuilder {

    private static ExportRequestBuilder instance = new ExportRequestBuilder();
    
    private boolean shardingEnabled = false;
    
    public static ExportRequestBuilder getInstance() {
        return instance;
    }
    
    public static void setInstance(ExportRequestBuilder instance) {
        ExportRequestBuilder.instance = instance;
    }

    public <T> ExportRequest<T> build(T data, StdProcessContext processContext) {
        ExportRequest<T> request = new ExportRequest<T>(data);
        if (shardingEnabled) {
            if (processContext != null) {
                Map<String, Object> ext = new HashMap<>();
                if (processContext.getProcessType() != null) {
                    ext.put(ShardingConstants.EXPORT_REQUEST_EXT_KEY_PROCESS_TYPE, processContext.getProcessType());
                    ext.put(ShardingConstants.EXPORT_REQUEST_EXT_KEY_BIZ_NO, processContext.getBizNo());
                }
                request.setExt(ext);
            }
        }
        return request;
    }

    public boolean isShardingEnabled() {
        return shardingEnabled;
    }

    public void setShardingEnabled(boolean shardingEnabled) {
        this.shardingEnabled = shardingEnabled;
    }
    
    

}
