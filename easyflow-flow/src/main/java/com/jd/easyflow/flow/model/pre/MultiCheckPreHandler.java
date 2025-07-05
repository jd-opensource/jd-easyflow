package com.jd.easyflow.flow.model.pre;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.LockUtil;

/**
 * 
 * IMPORTANT NOTICE! This class should not be singleton!
 * @author liyuliang5
 *
 */
public class MultiCheckPreHandler implements NodePreHandler, NodePrePropertyGetter {

    private static final Logger logger = LoggerFactory.getLogger(MultiCheckPreHandler.class);

    private List<String> preNodes;

    public MultiCheckPreHandler() {

    }

    public MultiCheckPreHandler(List<String> preNodes) {
        this.preNodes = preNodes;
    }

    /**
     * Judge all pre nodes finished.
     * 
     */
    @Override
    public boolean preHandle(NodeContext nodeContext, final FlowContext context) {
        Boolean checkResult = nodeContext.get(FlowConstants.NODECTX_PRE_RESULT);
        if (checkResult != null) {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Pre result checked:" + checkResult);
            }
            return checkResult;
        }
        boolean result = false;
        List<NodeContext> previousNodes = null;
        Object lockObj = LockUtil.getFlowContextLock(FlowConstants.CTX_LOCK_PREFIX + nodeContext.getNodeId(), context);
        synchronized (lockObj) {
            List<String> preNodes = context.get(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId());
            previousNodes = context.get(FlowConstants.CTX_PREVIOUS_NODES_PREFIX + nodeContext.getNodeId());
            if (preNodes == null) {
                preNodes = new ArrayList<String>();
                context.put(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId(), preNodes);
                previousNodes = new ArrayList<NodeContext>();
                context.put(FlowConstants.CTX_PREVIOUS_NODES_PREFIX + nodeContext.getNodeId(), previousNodes);
            }
            preNodes.add(nodeContext.getPreviousNode().getNodeId());
            previousNodes.add(nodeContext.getPreviousNode());

            FlowNode currentNode = context.getFlow().getNode(nodeContext.getNodeId());
            List<String> preNodeList = this.getPreNodes(nodeContext, context);
            
            List<String> configPreNodes = preNodeList != null ? preNodeList
                    : currentNode.getProperty(FlowConstants.PROP_PRENODES);
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Pre nodes executed:" + preNodes);
            }
            
            if (preNodes.size() < configPreNodes.size()) {
                return false;
            }
            for (String s : configPreNodes) {
                if (!preNodes.contains(s)) {
                    return false;
                }
            }
            result = true;
        }
        if (result) {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("All pre nodes finished");
            }
            context.remove(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId());
            context.remove(FlowConstants.CTX_PREVIOUS_NODES_PREFIX + nodeContext.getNodeId());
            nodeContext.put(FlowConstants.NODECTX_PREVIOUS_NODES, previousNodes);
            NodePreHandlerHelper.setNextNodesOfPreviousNode(previousNodes, nodeContext);
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
        return FlowConstants.NODE_PRE_CHECK_TYPE_MULTICHECK;
    }

    public List<String> getPreNodes(NodeContext nodeContext, FlowContext flowContext) {
        return this.preNodes;
    }

}
