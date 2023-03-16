package com.jd.easyflow.flow.model.builder;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.node.NodeImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowBuilder {

	private Flow flow;

	public static FlowBuilder create(String id, String name) {
		Flow flow = new Flow();
		flow.setId(id);
		flow.setName(name);
		FlowBuilder builder = new FlowBuilder();
		builder.flow = flow;
		return builder;
	}

	public FlowBuilder setProperty(String key, Object value) {
		flow.setProperty(key, value);
		return this;
	}
	
	public FlowBuilder addNode(String nodeId, NodeAction action) {
		return addNode(nodeId, action, null);
	}

	public FlowBuilder addNode(String nodeId, NodeAction action, NodePostHandler postHandler) {
		NodeImpl node = new NodeImpl();
		node.setId(nodeId);
		node.setAction(action);
		node.setPostHandler(postHandler);
		flow.addNode(node);
		return this;
	}
	
	public FlowBuilder addNode(FlowNode node) {
        flow.addNode(node);
        return this;
    }
	
    public FlowBuilder setStartNodeId(String startNodeId) {
        flow.setStartNodeIds(new String[] {startNodeId});
        return this;
    }
	
    public FlowBuilder setStartNodeIds(String[] startNodeIds) {
        flow.setStartNodeIds(startNodeIds);
        return this;
    }

	public Flow build() {
		return flow;
	}

}
