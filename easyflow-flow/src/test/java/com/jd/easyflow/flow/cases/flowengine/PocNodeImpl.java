package com.jd.easyflow.flow.cases.flowengine;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;

/**
 * This is only a POC demo of FlowNode.
 * 
 * @author liyuliang5
 */
public abstract class PocNodeImpl implements FlowNode {

    protected NodePreHandler preHandler;

    protected NodeAction action;

    protected NodePostHandler postHandler;

    @Override
    public NodeContext execute(NodeContext nodeContext, FlowContext context) {
        boolean preResult = true;
        if (preHandler != null) {
            preResult = preHandler.preHandle(nodeContext, context);
            nodeContext.setPreResult(preResult);
        }
        if (!preResult) {
            return nodeContext;
        }

        if (action != null) {
            Object result = action.execute(nodeContext, context);
            nodeContext.setActionResult(result);
        }

        if (postHandler != null) {
            NodeContext[] nextNodes = postHandler.postHandle(nodeContext, context);
            if (nextNodes != null) {
                nodeContext.setNextNodes(nextNodes);
            }
        }

        return nodeContext;
    }

}
