package com.jd.easyflow.flow.el;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ElEvaluator {
    
    /**
     * Evaluate with default context.
     * @param <T>
     * @param exp
     * @param root
     * @param cache
     * @return
     */
    <T> T evalWithDefaultContext(String exp, Object root, boolean cache);
    
    /**
     * Evaluate el value.
     * @param <T>
     * @param exp
     * @param nodeContext
     * @param flowContext
     * @param data
     * @return
     */
    <T>T eval(String exp, NodeContext nodeContext, FlowContext flowContext, Map<String, Object> data);
}
