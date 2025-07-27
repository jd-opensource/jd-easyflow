package com.jd.easyflow.flow.ext.serialize;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowParam;

/**
 * @author liyuliang5
 */
public interface FlowParamSerializer {

    public String serialize(FlowParam flowParam, Map<String, Object> config);
    
    public FlowParam deserialize(String s, Map<String, Object> config);
    
}
