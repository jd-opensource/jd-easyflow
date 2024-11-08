package com.jd.easyflow.process.client.task.flow;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 *
 * @author liyuliang5
 *
 */
public class TaskCreateNodeAction extends BaseTaskNodeAction {
    
    private static final Logger log = LoggerFactory.getLogger(TaskCreateNodeAction.class);


    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        return super.createTask(nodeContext, context);
    }

}
