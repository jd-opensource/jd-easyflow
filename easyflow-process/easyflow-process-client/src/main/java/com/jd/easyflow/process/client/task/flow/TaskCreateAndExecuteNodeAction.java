package com.jd.easyflow.process.client.task.flow;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 *
 */
public class TaskCreateAndExecuteNodeAction extends BaseTaskNodeAction {

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        super.createTask(nodeContext, context);
        return super.executeTask(nodeContext, context);
    }
    

}
