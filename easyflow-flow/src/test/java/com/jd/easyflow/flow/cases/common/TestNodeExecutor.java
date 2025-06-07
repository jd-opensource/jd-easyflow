package com.jd.easyflow.flow.cases.common;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;

/**
 * @author liyuliang5
 */
public class TestNodeExecutor implements NodeExecutor {
    
    private Object value;
    
    public TestNodeExecutor(Object value) {
        this.value = value;
    }

    @Override
    public Object execute(NodeContext nodeContext, FlowContext context) {
        return value;
    }

}
