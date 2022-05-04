package com.jd.easyflow.flow.cases.posthandler;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class PostParamNode1Action implements NodeAction {

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        nodeContext.put("node1Param", 123);
        return null;
    }

}
