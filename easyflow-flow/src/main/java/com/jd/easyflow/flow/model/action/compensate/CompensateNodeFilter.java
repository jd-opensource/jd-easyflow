package com.jd.easyflow.flow.model.action.compensate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeContextAccessor;
import com.jd.easyflow.flow.model.filter.impl.BaseNodeFilter;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.LockUtil;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public class CompensateNodeFilter extends BaseNodeFilter {

    private static final Logger logger = LoggerFactory.getLogger(CompensateNodeFilter.class);
    
    private static final String CTX_COMENSATED_END_NODES_LOCK_KEY = "_compensated_end_nodes_lock";
    private static final String NODE_CTX_COMENSATING_LOCK_KEY = "_compensating_lock";

        
    public CompensateNodeFilter() {
        
    }
    
    public CompensateNodeFilter(int order) {
        this.order = order;
    }

    @Override
    public NodeContext doFilter(Triple<FlowNode, NodeContext, FlowContext> request,
            FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain) {
        FlowContext context = request.getRight();
        NodeContext nodeContext = request.getMiddle();
        if (! Boolean.TRUE.equals(nodeContext.get(FlowConstants.NODECTX_COMPENSATE_NODE_FLAG))) {
            // Normal scenario.
            NodeContext newNodeContext = chain.doFilter(request);
            if (CompensateHelper.isCompensating(context)) {
                // post to previous node.
                List<NodeContext> compensatedNextNodes = new ArrayList<NodeContext>();
                // current node previous nodes.
                compensatedNextNodes.add(CompensateHelper.createCompensateNode(newNodeContext));
                
                if (!Boolean.TRUE.equals(context.get(CompensateHelper.FLOW_CONTEXT_COMPENSATE_END_NODES_FLAG))) {
                    Object lockObj = LockUtil.getFlowContextLock(CTX_COMENSATED_END_NODES_LOCK_KEY, context);
                    synchronized (lockObj) {
                        // add end nodes.
                        if (!Boolean.TRUE.equals(context.get(CompensateHelper.FLOW_CONTEXT_COMPENSATE_END_NODES_FLAG))) {
                            Set<NodeContext> endNodes = new HashSet<NodeContext>();
                            getEndNodes(context.getStartNodes().toArray(new NodeContext[context.getStartNodes().size()]), endNodes);
                            for (NodeContext endNode : endNodes) {
                                compensatedNextNodes.add(CompensateHelper.createCompensateNode(endNode));
                            }
                            context.put(CompensateHelper.FLOW_CONTEXT_COMPENSATE_END_NODES_FLAG, true);
                        }
                        NodeContextAccessor.setNextNodes(newNodeContext,
                                compensatedNextNodes.toArray(new NodeContext[compensatedNextNodes.size()]));
                    }
                }
            }
            newNodeContext.put(CompensateNodeFilter.class.getName(), true);
            return newNodeContext; 
        } else {
            // Compensate scenario.
            NodeImpl nodeImpl = (NodeImpl) context.getFlow().getNode(nodeContext.getNodeId());
            NodeContext originNodeCtx = nodeContext.get(FlowConstants.NODECTX_COMPENSATE_FOR);
            Object lockObj = LockUtil.getNodeContextLock(NODE_CTX_COMENSATING_LOCK_KEY, originNodeCtx);
            synchronized (lockObj) {
                // Judge compensate
                if (Boolean.TRUE.equals(originNodeCtx.get(CompensateHelper.NODE_CONTEXT_COMPENSATING_FLAG))) {
                    if (context.isLogOn() && logger.isInfoEnabled()) {
                        logger.info("Node:" + originNodeCtx.getNodeId() + "has been compensated, return");
                    }
                    return nodeContext;
                }
                
                List<NodeContext> originNextNodes = originNodeCtx.get(FlowConstants.NODECTX_NEXT_NODES);
                if (originNextNodes == null && originNodeCtx.getNextNodes() != null) {
                    originNextNodes = Arrays.asList(originNodeCtx.getNextNodes());
                }
                if (originNextNodes != null && originNextNodes.size() > 0) {
                    String notCompensated = null;
                    for (NodeContext next : originNextNodes) {
                        if (Boolean.TRUE.equals(next.get(FlowConstants.NODECTX_COMPENSATE_NODE_FLAG))) {
                            continue;
                        }
                        if (!Boolean.TRUE.equals(next.get(FlowConstants.NODECTX_COMPENSATED_FLAG))) {
                            notCompensated = next.getNodeId();
                            break;
                        }
                    }
                    if (notCompensated != null) {
                        if (context.isLogOn() && logger.isInfoEnabled()) {
                            logger.info("Next nodes of " + originNodeCtx.getNodeId() + " not all compensated:" + notCompensated);
                        }
                        return nodeContext;
                    }
                }
                originNodeCtx.put(CompensateHelper.NODE_CONTEXT_COMPENSATING_FLAG, true);
            }
                
            // Execute compensate action
            if (Boolean.FALSE.equals(originNodeCtx.getPreResult())) {
                if (context.isLogOn() && logger.isInfoEnabled()) {
                    logger.info("Node:" + originNodeCtx.getNodeId() + " pre result is false, skip compensate action.");
                }
            } else {
                NodeAction nodeAction = nodeImpl.getAction();
                Object compensateResult = null;
                NodeAction compensateNodeAction = (NodeAction) nodeImpl
                        .getProperty(FlowConstants.PROP_RUNTIME_COMPENSATE_ACTION);
                if (compensateNodeAction != null) {
                    if (context.isLogOn() && logger.isInfoEnabled()) {
                        logger.info("Compensate using NodeAction:" + compensateNodeAction.getClass());
                    }
                    compensateResult = compensateNodeAction.execute(nodeContext, context);
                } else if (nodeAction instanceof CompensateAction) {
                    if (context.isLogOn() && logger.isInfoEnabled()) {
                        logger.info("Compensate using CompensateAction");
                    }
                    compensateResult = ((CompensateAction) nodeAction).compensate(nodeContext, context);
                }
                NodeContextAccessor.setActionResult(nodeContext, compensateResult);
            }

            originNodeCtx.put(FlowConstants.NODECTX_COMPENSATED_FLAG, true);

            // post to previous node.
            List<NodeContext> previousList = originNodeCtx.get(FlowConstants.NODECTX_PREVIOUS_NODES);
            if (previousList != null) {
                // MultiCheck and InclusiveCheck
                if (previousList.size() == 0) {
                    NodeContextAccessor.setNextNodes(nodeContext, null);
                } else {
                    NodeContext[] previouses = new NodeContext[previousList.size()];
                    for (int i = 0; i < previousList.size(); i++) {
                        previouses[i] = CompensateHelper.createCompensateNode(previousList.get(i));
                    }
                    NodeContextAccessor.setNextNodes(nodeContext, previouses);
                }
            } else if (originNodeCtx.getPreviousNode() != null) {
                NodeContextAccessor.setNextNodes(nodeContext,
                        new NodeContext[] { CompensateHelper.createCompensateNode(originNodeCtx.getPreviousNode()) });
            } else {
                NodeContextAccessor.setNextNodes(nodeContext, null);
            }
            return nodeContext;
        } 
    }


    private void getEndNodes(NodeContext[] nodes, Set<NodeContext> result) {
        if (nodes == null || nodes.length == 0) {
            return;
        }
        for (NodeContext node : nodes) {
            if (!Boolean.TRUE.equals(node.get(CompensateNodeFilter.class.getName()))) {
                continue;
            }
            if (node.getNextNodes() == null || node.getNextNodes().length == 0) {
                result.add(node);
            } else {
                getEndNodes(node.getNextNodes(), result);
            }
        }
    }
  
}