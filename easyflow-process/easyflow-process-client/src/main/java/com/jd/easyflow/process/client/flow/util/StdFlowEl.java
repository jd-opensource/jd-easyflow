package com.jd.easyflow.process.client.flow.util;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class StdFlowEl {
    
    public static Object instance(FlowContext context) {
        ProcessInstanceDTO instance = context.get(StdFlowProcessConstants.FLOW_CTX_INSTANCE);
        return instance;
    }

    public static Object bizData(FlowContext context, String key) {
        ProcessInstanceDTO instance = context.get(StdFlowProcessConstants.FLOW_CTX_INSTANCE);
        String bizData = instance.getBizData();
        Map<String, Object> map = JSON.parseObject(bizData, Map.class);
        if (map == null) {
            return null;
        }
        return map.get(key);
    }
    
}
