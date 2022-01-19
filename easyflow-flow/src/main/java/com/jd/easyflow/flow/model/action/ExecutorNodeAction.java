package com.jd.easyflow.flow.model.action;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExecutorNodeAction implements NodeAction {

	private NodeExecutor executor;

	public ExecutorNodeAction() {
	}

	public ExecutorNodeAction(NodeExecutor executor) {
		this.executor = executor;
	}

	@Override
	public <T> T execute(NodeContext nodeContext, FlowContext context) {
		Object result = executor.execute(nodeContext, context);
		return (T) result;

	}

	public NodeExecutor getExecutor() {
		return executor;
	}

	public void setExecutor(NodeExecutor executor) {
		this.executor = executor;
	}

}
