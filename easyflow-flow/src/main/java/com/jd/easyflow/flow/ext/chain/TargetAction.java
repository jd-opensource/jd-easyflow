package com.jd.easyflow.flow.ext.chain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.action.ParamExecutorNodeAction;

/**
 * 
 * @author liyuliang5
 *
 */
public class TargetAction extends ParamExecutorNodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(TargetAction.class);

    @Override
    public Void execute(NodeContext nodeContext, FlowContext context) {
        try {
            Object result = super.execute(nodeContext, context);
            context.getResult().setResult(result);
            return null;
        } catch (Throwable t) {
            logger.error("Target action execute exception:" + t.getMessage(), t);
            context.put(ChainConstants.EXCEPTION, t);
            return null;
        } finally {
            context.put(ChainConstants.STAGE, ChainConstants.STAGE_POST);
        }
    }
}
