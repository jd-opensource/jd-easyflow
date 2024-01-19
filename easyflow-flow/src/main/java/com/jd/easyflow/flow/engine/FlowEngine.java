package com.jd.easyflow.flow.engine;

import java.util.Map;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FlowEngine {
    
    /**
     * Execute flow.
     * @param param
     * @return
     */
    FlowResult execute(FlowParam param);
    
    /**
     * Get flow definition.
     * @param id
     * @return
     */
    Flow getFlow(String id);
    
    /**
     * Get flow parser.
     * @return
     */
    FlowParser getFlowParser();
    
    
    /**
     * Get engine properties.
     * @return
     */
    Map<String, Object> getProperties();
    
    
    /**
     * Get engine property.
     * @param <T>
     * @param key
     * @return
     */
    <T>T getProperty(String key);
    
}
