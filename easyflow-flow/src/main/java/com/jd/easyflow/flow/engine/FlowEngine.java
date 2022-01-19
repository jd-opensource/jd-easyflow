package com.jd.easyflow.flow.engine;

import com.jd.easyflow.flow.model.Flow;

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
}
