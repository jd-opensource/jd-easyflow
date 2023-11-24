package com.jd.easyflow.flow.engine;

import com.jd.easyflow.flow.model.FlowLifeCycle;

/**
 * Flow runner. 
 * @author liyuliang5
 * @date 2021/07/26
 */
public interface FlowRunner extends FlowLifeCycle {
    
    /**
     * Run flow.
     * @param context
     */
    void run(FlowContext context);
    
}
