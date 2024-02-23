package com.jd.easyflow.flow.engine.impl;

import org.apache.commons.lang3.tuple.Triple;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowRunner;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowEventTypes;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class BaseFlowRunner implements FlowRunner {

    private static final Logger logger = LoggerFactory.getLogger(BaseFlowRunner.class);

    @Override
    public void run(FlowContext context) {
        Flow flow = context.getFlow();
        flow.triggerEvent(FlowEventTypes.RUN_START, context);
        if (flow.getPreHandler() != null) {
            boolean preResult = flow.getPreHandler().preHandle(context);
            ((FlowContextImpl) context).setPreResult(preResult);
            if (!preResult) {
                flow.triggerEvent(FlowEventTypes.RUN_END, context);
                return;
            }
        }
        runNodes((FlowContextImpl) context);
        if (flow.getPostHandler() != null) {
            flow.getPostHandler().postHandle(context);
        }
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
            runNode(node, currentNode, context);
            // get next nodes
            nextNodes = currentNode.getNextNodes();
        } catch (Throwable t) { // NOSONAR
            currentNode.setThrowable(t);
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
            currentNode.setPreviousNode(null);
            currentNode.setNextNodes(null);
        }
        return nextNodes;
    }

    protected void runNode(FlowNode node, NodeContext currentNode, FlowContextImpl context) {
        Flow flow = context.getFlow();
        if (flow.getNodeFilters() == null || flow.getNodeFilters().size() == 0) {
            invokeNode(node, currentNode, context, flow);
            return;
        }
        FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain = new FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>(
                flow.getNodeFilters(), p -> {
                    return invokeNode(node, currentNode, context, flow);
                });
        chain.doFilter(Triple.of(node, currentNode, context));

    }

    private NodeContext invokeNode(FlowNode node, NodeContext currentNode, FlowContext context, Flow flow) {
        Throwable throwable = null;
        try {
            flow.triggerEvent(FlowEventTypes.NODE_START, currentNode, context, false);
            // Execute node
            NodeContext nodeContext = node.execute(currentNode, context);
            flow.triggerEvent(FlowEventTypes.NODE_END, currentNode, context, false);
            return nodeContext;
        } catch (Throwable t) {// NOSONAR
            throwable = t;
            logger.error("Flow node execute exception, Node:" + currentNode.getNodeId() + "," + t.getMessage());
            throw t;
        } finally {
            currentNode.setThrowable(throwable);
            flow.triggerEvent(FlowEventTypes.NODE_COMPLETE, currentNode, context, true);
        }
    }
}
