package com.jd.easyflow.flow.engine;

/**
 * Flow runner. 
 * @author liyuliang5
 * @date 2021/07/26
 */
public interface FlowRunner {
    
    /**
     * Run flow.
     * @param context
     */
    void run(FlowContext context);
}
