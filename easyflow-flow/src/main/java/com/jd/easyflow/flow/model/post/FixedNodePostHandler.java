package com.jd.easyflow.flow.model.post;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FixedNodePostHandler extends AbstractNodePostHandler {
    
    private Object to;
    
    public FixedNodePostHandler() {
        
    }

	public FixedNodePostHandler(Object to) {
	    this.to = to;
	}

	@Override
	public NodeContext[] postHandle(NodeContext nodeContext, FlowContext context) {
	    return nodeIds2Nodes(parseToNodeIds(to, nodeContext, context));
	}

    public Object getTo() {
        return to;
    }

    public void setTo(Object to) {
        this.to = to;
    }
	
	

}