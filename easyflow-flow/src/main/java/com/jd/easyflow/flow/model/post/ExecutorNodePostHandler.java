package com.jd.easyflow.flow.model.post;

import java.util.List;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExecutorNodePostHandler extends AbstractNodePostHandler {
	
	private NodeExecutor<? extends Object> executor;
	
	public ExecutorNodePostHandler(NodeExecutor<? extends Object> executor) {
		this.executor = executor;
	}

	@Override
	public NodeContext[] postHandle(NodeContext nodeContext, FlowContext context) {
		List<String> nodeIds = (List<String>) executor.execute(nodeContext, context);
		return nodeIds2Nodes(nodeIds);
	}

}
