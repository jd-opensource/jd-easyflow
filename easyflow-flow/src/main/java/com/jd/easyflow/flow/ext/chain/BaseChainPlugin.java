package com.jd.easyflow.flow.ext.chain;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * Node action with chain pattern.
 * 
 * @author liyuliang5
 *
 */
public abstract class BaseChainPlugin implements NodeAction {

    /**
     * execute node action.
     */
    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        String stage = context.get(ChainConstants.STAGE);
        if (stage == null) {
            stage = ChainConstants.STAGE_PRE;
            context.put(ChainConstants.STAGE, ChainConstants.STAGE_PRE);
        }
        if (ChainConstants.STAGE_PRE.equals(stage)) {
            try {
                boolean result = preHandle(nodeContext, context);
                if (result == false) {
                    context.put(ChainConstants.STAGE, ChainConstants.STAGE_POST);
                }
            } catch (Throwable t) {
                context.put(ChainConstants.STAGE, ChainConstants.STAGE_POST);
                context.put(ChainConstants.EXCEPTION, t);
            }
        } else {
            try {
                postHandle(nodeContext, context);
            } catch (Throwable t) {
                context.put(ChainConstants.EXCEPTION, t);
            }
        }
        return null;
    }

    /**
     * Pre handle.
     * 
     * @param nodeContext
     * @param context
     * @return
     */
    public abstract boolean preHandle(NodeContext nodeContext, FlowContext context);

    /**
     * Post handle.
     * 
     * @param nodeContext
     * @param context
     */
    public void postHandle(NodeContext nodeContext, FlowContext context) {
        Throwable t = context.get(ChainConstants.EXCEPTION);
        if (t == null) {
            postHandleNormal(nodeContext, context);
        } else {
            postHandleException(t, nodeContext, context);
        }
    }

    /**
     * Normal post handle.
     * 
     * @param nodeContext
     * @param context
     */
    public abstract void postHandleNormal(NodeContext nodeContext, FlowContext context);

    /**
     * Post handle when exception.
     * 
     * @param t
     * @param nodeContext
     * @param context
     */
    public abstract void postHandleException(Throwable t, NodeContext nodeContext, FlowContext context);

    /**
     * Clear exception.
     * 
     * @param nodeContext
     * @param context
     */
    public void clearException(NodeContext nodeContext, FlowContext context) {
        context.put(ChainConstants.EXCEPTION, null);
    }

}
