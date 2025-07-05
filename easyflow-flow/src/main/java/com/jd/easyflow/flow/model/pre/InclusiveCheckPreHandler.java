package com.jd.easyflow.flow.model.pre;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeContextAccessor;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.model.filter.impl.BaseNodeFilter;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowNodeLinkUtil;
import com.jd.easyflow.flow.util.LockUtil;
import com.jd.easyflow.flow.util.Triple;

/**
 * Inclusive check pre handler.
 * 
 *  IMPORTANT NOTICE! This class should not be singleton!
 * 
 * @author liyuliang5
 *
 */
public class InclusiveCheckPreHandler implements NodePreHandler, NodePrePropertyGetter {

    private static final Logger logger = LoggerFactory.getLogger(InclusiveCheckPreHandler.class);
    
    private static final String CTX_INCLUSIVE_LOCK = "_inclusive_lock";


    private List<String> preNodes;

    public InclusiveCheckPreHandler() {
    }

    public InclusiveCheckPreHandler(List<String> preNodes) {
        this.preNodes = preNodes;
    }
    
    @Override
    public boolean preHandle(NodeContext nodeContext, final FlowContext context) {
        Boolean checkResult = nodeContext.get(FlowConstants.NODECTX_PRE_RESULT);
        if (checkResult != null) {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Pre result checked:" + checkResult);
            }
            return checkResult;
        }
        
        NodeContext previousNode = nodeContext.getPreviousNode();
        if (previousNode == null) {
            return false;
        }
        String previousNodeId = previousNode.getNodeId();
        boolean result = false;
        Map<String, WaitingNodeInfo> map = null;
        WaitingNodeInfo waitingNodeInfo = null;
        
        
        Object lockObj = LockUtil.getFlowContextLock(CTX_INCLUSIVE_LOCK, context);
        synchronized (lockObj) {
            map = context.get(InclusiveCheckHelper.CTX_WAITING_NODE_MAP);
            if (map == null) {
                map = new HashMap<String, WaitingNodeInfo>();
                context.put(InclusiveCheckHelper.CTX_WAITING_NODE_MAP, map);
            }
            waitingNodeInfo = map.get(nodeContext.getNodeId());
            if (waitingNodeInfo == null) {
                waitingNodeInfo = new WaitingNodeInfo();
                waitingNodeInfo.waitNodeId = nodeContext.getNodeId();
                waitingNodeInfo.unknownPreNodes = new HashSet<String>();
                waitingNodeInfo.finishedPreNodes = new HashSet<String>();
                waitingNodeInfo.unreachablePreNodes = new HashSet<String>();
                waitingNodeInfo.previousNodes = new ArrayList<NodeContext>();
                
                FlowNode currentNode = context.getFlow().getNode(nodeContext.getNodeId());
                List<String> preNodeList = this.getPreNodes(nodeContext, context);
                List<String> configPreNodes = preNodeList != null ? preNodeList
                        : currentNode.getProperty(FlowConstants.PROP_PRENODES);
                if (! configPreNodes.contains(previousNodeId)) {
                    if (context.isLogOn() && logger.isDebugEnabled()) {
                        logger.info("Node:" + previousNodeId + " not in check list" );
                    }
                    return false;
                }
                for (String preNode : configPreNodes) {
                    if (FlowNodeLinkUtil.isReachable(preNode, nodeContext.getNodeId(), context.getFlow())) {
                        waitingNodeInfo.unknownPreNodes.add(preNode);
                    } else {
                        waitingNodeInfo.unreachablePreNodes.add(preNode);
                    }
                }
                map.put(nodeContext.getNodeId(), waitingNodeInfo);
            }
            waitingNodeInfo.previousNodes.add(previousNode);
            waitingNodeInfo.finishedPreNodes.add(previousNodeId);
            waitingNodeInfo.unknownPreNodes.remove(previousNodeId);
            if (waitingNodeInfo.unknownPreNodes.size() == 0) {
                result = true;
            } else {
                InclusiveCheckHelper.judgeOneWaitingNode(waitingNodeInfo, context);
                if (waitingNodeInfo.unknownPreNodes.size() == 0) {
                    result = true;
                } else {
                    if (context.isLogOn() && logger.isDebugEnabled()) {
                        logger.debug("Finish nodes:" + waitingNodeInfo.finishedPreNodes + " Unreachable nodes:" + waitingNodeInfo.unreachablePreNodes + " Unknown nodes:" + waitingNodeInfo.unknownPreNodes);
                    }
                    return false;
                }
            }
            
        }
        if (result) {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Node:" + nodeContext.getNodeId() + " is activated");
            }
            map.remove(nodeContext.getNodeId());
            nodeContext.put(FlowConstants.NODECTX_PREVIOUS_NODES, waitingNodeInfo.previousNodes);
            NodePreHandlerHelper.setNextNodesOfPreviousNode(waitingNodeInfo.previousNodes, nodeContext);
        }
        return result;
    }

    public List<String> getPreNodes() {
        return preNodes;
    }

    public void setPreNodes(List<String> preNodes) {
        this.preNodes = preNodes;
    }

    @Override
    public String getCheckType() {
        return FlowConstants.NODE_PRE_CHECK_TYPE_INCLUSIVECHECK;
    }

    @Override
    public List<String> getPreNodes(NodeContext nodeContext, FlowContext flowContext) {
        return this.preNodes;
    }
    
    @Override
    public void init(InitContext initContext, Object parent) {
        boolean recordHistory = ! Boolean.FALSE.equals(initContext.getFlow().getProperty(FlowConstants.FLOW_PROPERTY_RECORD_HISTORY));
        if (! recordHistory) {
            throw new FlowException("InclusiveCheck must record history");
        }
        Flow flow = initContext.getFlow();
        List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> filters = flow.getFilterManager().getNodeFilters();
        boolean contains = false;
        if (filters == null) {
            filters = new ArrayList<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>>();
            initContext.getFlow().getFilterManager().setNodeFilters(filters);
        }
        for (Filter filter : filters) {
            if (filter instanceof InclusiveCheckWaitNodeProcessFilter) {
                contains = true;
                break;
            }
        }
        if (!contains) {
            // after compensate filter.
            filters.add(0, new InclusiveCheckWaitNodeProcessFilter(Integer.MAX_VALUE - 1));
            initContext.getFlow().getFilterManager().setNodeFilters(filters);
        }
    }
    
    public static class InclusiveCheckWaitNodeProcessFilter extends BaseNodeFilter {
        
        private static final Logger logger = LoggerFactory.getLogger(InclusiveCheckWaitNodeProcessFilter.class);
        
        public InclusiveCheckWaitNodeProcessFilter(int order) {
            this.order = order;
        }

        @Override
        public NodeContext doFilter(Triple<FlowNode, NodeContext, FlowContext> request,
                FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain) {
            chain.doFilter(request);
            NodeContext nodeContext = request.getMiddle();
            FlowContext flowContext = request.getRight();
            Object lockObj = LockUtil.getFlowContextLock(CTX_INCLUSIVE_LOCK, flowContext);
            synchronized (lockObj) {
                nodeContext.put(InclusiveCheckWaitNodeProcessFilter.class.getName(), true);
                Map<String, WaitingNodeInfo> map = flowContext.get(InclusiveCheckHelper.CTX_WAITING_NODE_MAP);
                if (map != null && !map.isEmpty()) {
                    List<NodeContext> additionalNextNodes = null;
                    for (WaitingNodeInfo info : map.values()) {
                        if (info.waitNodeId.equals(nodeContext.getNodeId())) {
                            continue;
                        }
                        InclusiveCheckHelper.judgeOneWaitingNode(info, flowContext);
                        if (info.unknownPreNodes.isEmpty()) {
                            if (flowContext.isLogOn() && logger.isInfoEnabled()) {
                                logger.info("Node:" + info.waitNodeId + " is activated in filter");
                            }
                            if (additionalNextNodes == null) {
                                additionalNextNodes = new ArrayList<NodeContext>();
                                if (nodeContext.getNextNodes() != null) {
                                    for (NodeContext nc : nodeContext.getNextNodes()) {
                                        additionalNextNodes.add(nc);
                                    }
                                }
                            }
                            NodeContext additionNode = new NodeContext(info.waitNodeId);
                            additionalNextNodes.add(additionNode);
                        }
                    }
                    if (additionalNextNodes != null) {
                        NodeContextAccessor.setNextNodes(nodeContext, additionalNextNodes.toArray(new NodeContext[additionalNextNodes.size()]));
                    }
                }
            }
            return nodeContext;
        }
    }
    
    private static class InclusiveCheckHelper {
        
        
        static final String CTX_WAITING_NODE_MAP = "_WAITING_NODE_MAP";
        static final String CTX_RUNNING_NODES = "_RUNNING_NODES";
        
        private static final List<String> EMPTY_LIST = new ArrayList<String>();

        static void judgeOneWaitingNode(WaitingNodeInfo info, FlowContext context) {
            Set<NodeContext> runningNodes = refreshRunningNodes(context);
            Map<String, WaitingNodeInfo> map = context.get(CTX_WAITING_NODE_MAP);
            Iterator<String> iterator = info.unknownPreNodes.iterator();
            while (iterator.hasNext()) {
                String unknownPreNode = iterator.next();
                boolean reachable = false;
                for (NodeContext nodeContext : runningNodes) {
                    if (nodeContext.getNodeId().equals(unknownPreNode)) {
                        reachable = true;
                        break;
                    }
                    if (nodeContext.getNodeId().equals(info.waitNodeId)) {
                        if (nodeContext.getPreviousNode() != null && nodeContext.getPreviousNode().getNodeId().equals(unknownPreNode)) {
                            reachable = true;
                            break;
                        } else {
                            continue;
                        }
                    }
                    if (FlowNodeLinkUtil.isReachable(nodeContext.getNodeId(), unknownPreNode, context.getFlow())) {
                        reachable = true;
                        break;
                    }
                }
                if (! reachable) {
                    for (String key : map.keySet()) {
                        if (key.equals(info.waitNodeId)) {
                            continue;
                        }
                        if (key.equals(unknownPreNode)) {
                            reachable = true;
                            break;
                        }
                        if (FlowNodeLinkUtil.isReachable(key, unknownPreNode, context.getFlow())) {
                            reachable = true;
                            break;
                        }
                    }
                }
                if (! reachable) {
                    info.unreachablePreNodes.add(unknownPreNode);
                    iterator.remove();

                }
            }
        }
        
        
        private static Set<NodeContext> refreshRunningNodes(FlowContext context) {
            Set<NodeContext> runningNodes = context.get(CTX_RUNNING_NODES);
            if (runningNodes == null) {
                runningNodes = new HashSet<NodeContext>();
                runningNodes.addAll(context.getStartNodes());
                context.put(CTX_RUNNING_NODES, runningNodes);
            }
            List<NodeContext> addList = null;
            List<NodeContext> removeList = null;
            for (NodeContext node : runningNodes) {
                if (Boolean.TRUE.equals(node.get(InclusiveCheckWaitNodeProcessFilter.class.getName()))) {
                    if (node.getNextNodes() != null) {
                        for (NodeContext nctx : node.getNextNodes()) {
                            if (addList == null) {
                                addList = new ArrayList<NodeContext>();
                            }
                            addList.add(nctx);
                        }
                    }
                    if (removeList == null) {
                        removeList = new ArrayList<NodeContext>();
                    }
                    removeList.add(node);
                }
            }
            if (removeList != null) {
                runningNodes.removeAll(removeList); 
            }
            if (addList != null) {
                runningNodes.addAll(addList);
            }
            if (addList != null && !addList.isEmpty()) {
                refreshRunningNodes(context);
            }
            return runningNodes;
        }
        
    }

    private static class WaitingNodeInfo {
        
        String waitNodeId;
        
        Set<String> finishedPreNodes;
        Set<String> unreachablePreNodes;
        Set<String> unknownPreNodes;
        List<NodeContext> previousNodes;
    }
 
}





