package com.jd.easyflow.flow.engine.impl;

import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowRunner;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeContextAccessor;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.Triple;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class BaseFlowRunner implements FlowRunner {

    private static final Logger logger = LoggerFactory.getLogger(BaseFlowRunner.class);
    
    private Function<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> outerNodeInvoker = p -> invokeNode(p.getLeft(), p.getMiddle(), p.getRight());
    private Function<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> innerNodeInvoker = p -> p.getLeft().execute(p.getMiddle(), p.getRight());
    private Function<FlowContext, Boolean> outerFlowPreHandlerInvoker = p -> invokePreHandler(p.getFlow(), p);
    private Function<FlowContext, Boolean> innerFlowPreHandlerInvoker = p -> p.getFlow().getPreHandler().preHandle(p);
    private Function<FlowContext, Void> outerFlowPostHandlerInvoker = p -> {invokePostHandler(p.getFlow(), p); return null;};
    private Function<FlowContext, Void> innerFlowPostHandlerInvoker = p -> {p.getFlow().getPostHandler().postHandle(p); return null;};
    
    @Override
    public void run(FlowContext context) {
        Flow flow = context.getFlow();
        flow.triggerEvent(FlowEventTypes.RUN_START, context);
       if (! executePreHandler(flow, context)) {
           flow.triggerEvent(FlowEventTypes.RUN_END, context);
           return;
       }
        runNodes((FlowContextImpl) context);
        executePostHandler(flow, context);
        flow.triggerEvent(FlowEventTypes.RUN_END, context);
    }

    /**
     * Run flow.
     * 
     * @param context
     */
    public abstract void runNodes(FlowContextImpl context);

    protected NodeContext[] runOneNodeAndAddNextNodes(NodeContext currentNode, FlowContextImpl context) {
        NodeContext[] nextNodes = runOneNode(currentNode, context);
        if (nextNodes != null) {
            context.addNodes(nextNodes);
        }
        return nextNodes;
    }

    /**
     * Run one node.
     * 
     * @param currentNode
     * @param context
     * @param flow
     * @return next nodes
     */
    protected NodeContext[] runOneNode(NodeContext currentNode, FlowContextImpl context) {
        if (context.isLogOn() && logger.isInfoEnabled()) {
            logger.info("EXECUTE NODE:" + currentNode.getNodeId());
        }
        FlowNode node = context.getFlow().getNode(currentNode.getNodeId());
        if (node == null) {
            throw new FlowException("Node " + currentNode.getNodeId() + " not exists");
        }
        NodeContext[] nextNodes = null;
        try {
            currentNode = runNode(node, currentNode, context);
            // get next nodes
            nextNodes = currentNode.getNextNodes();
        } catch (Throwable t) { // NOSONAR
            NodeContextAccessor.setThrowable(currentNode, t);
            throw t;
        } finally {
            if (nextNodes == null) {
                ((FlowContextImpl) context).addEndNode(currentNode);
            }
        }
        // print nodes info
        if (context.isLogOn() && logger.isInfoEnabled()) {
            if (nextNodes == null || nextNodes.length == 0) {
                logger.info("NEXT NODES:");
            } else if (nextNodes.length == 1) {
                logger.info("NEXT NODES:" + nextNodes[0].getNodeId());
            } else {
                StringBuilder builder = new StringBuilder();
                for (NodeContext n : nextNodes) {
                    builder.append(n.getNodeId()).append(",");
                }
                logger.info("NEXT NODES:" + (builder.length() == 0 ? "" : builder.substring(0, builder.length() - 1)));
            }
        }
        // Clear previous node to avoid OOM
        if (!context.isRecordHistory()) {
            NodeContextAccessor.setPreviousNode(currentNode, null);
            NodeContextAccessor.setNextNodes(currentNode, null);
        }
        return nextNodes;
    }

    protected NodeContext runNode(FlowNode node, NodeContext currentNode, FlowContextImpl context) {
        Flow flow = context.getFlow();
        if (flow.getFilterManager().noOuterNodeFilter()) {
            return invokeNode(node, currentNode, context);
        }
        return flow.getFilterManager().doOuterNodeFilter(Triple.of(node, currentNode, context), outerNodeInvoker);

    }

    private NodeContext invokeNode(FlowNode node, NodeContext currentNode, FlowContext context) {
        Throwable throwable = null;
        Flow flow = context.getFlow();
        try {
            flow.triggerEvent(FlowEventTypes.NODE_START, currentNode, context, false);
            // Execute node
            if (flow.getFilterManager().noInnerNodeFilter()) {
                currentNode = node.execute(currentNode, context);
            } else {
                currentNode = flow.getFilterManager().doInnerNodeFilter(Triple.of(node, currentNode, context), innerNodeInvoker);
            }
            flow.triggerEvent(FlowEventTypes.NODE_END, currentNode, context, false);
            return currentNode;
        } catch (Throwable t) {// NOSONAR
            throwable = t;
            if (context.isLogOn() && logger.isErrorEnabled()) {
                logger.error("Flow node execute exception, Node:" + currentNode.getNodeId() + "," + t.getMessage());
            } 
            throw t;
        } finally {
            NodeContextAccessor.setThrowable(currentNode, throwable);
            flow.triggerEvent(FlowEventTypes.NODE_COMPLETE, currentNode, context, true);
        }
    }
    
    
    private boolean executePreHandler(Flow flow, FlowContext context) {
        if (flow.getFilterManager().noOuterFlowPreHandlerFilter()) {
            return invokePreHandler(flow, context);
        } else {
            Boolean preResult = flow.getFilterManager().doOuterFlowPreHandlerFilter(context, outerFlowPreHandlerInvoker);
            ((FlowContextImpl) context).setPreResult(preResult);
            return preResult == null ? true : preResult;
        }
    }
    
    private boolean invokePreHandler(Flow flow, FlowContext context) {
        if (flow.getPreHandler() != null) {
            flow.triggerEvent(FlowEventTypes.FLOW_PRE_START, context);
            boolean preResult;
            if (flow.getFilterManager().noInnerFlowPreHandlerFilter()) {
                preResult = flow.getPreHandler().preHandle(context);
            } else {
                Boolean result = flow.getFilterManager().doInnerFlowPreHandlerFilter(context, innerFlowPreHandlerInvoker);
                preResult = result == null ? true : result;
            }
            ((FlowContextImpl) context).setPreResult(preResult);
            flow.triggerEvent(FlowEventTypes.FLOW_PRE_END, context);
        }
        return context.getPreResult() == null ? true : context.getPreResult();
    }
    
    private void executePostHandler(Flow flow, FlowContext context) {
        if (flow.getFilterManager().noOuterFlowPostHandlerFilter()) {
            invokePostHandler(flow, context);
        } else {
            flow.getFilterManager().doOuterFlowPostHandlerFilter(context, outerFlowPostHandlerInvoker);
        }
    }
    
    
    private void invokePostHandler(Flow flow, FlowContext context) {
        if (flow.getPostHandler() != null) {
            flow.triggerEvent(FlowEventTypes.FLOW_POST_START, context);
            if (flow.getFilterManager().noInnerFlowPostHandlerFilter()) {
                flow.getPostHandler().postHandle(context);
            } else {
                flow.getFilterManager().doInnerFlowPostHandlerFilter(context, innerFlowPostHandlerInvoker); 
            }
            flow.triggerEvent(FlowEventTypes.FLOW_POST_END, context);
        }
    }
}
