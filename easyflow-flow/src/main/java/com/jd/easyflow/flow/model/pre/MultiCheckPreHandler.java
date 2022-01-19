package com.jd.easyflow.flow.model.pre;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class MultiCheckPreHandler  implements NodePreHandler {
    
    private static final Logger logger = LoggerFactory.getLogger(MultiCheckPreHandler.class);

    /**
     * Judge all pre nodes finished.
     * 
     */
    @Override
    public boolean preHandle(NodeContext nodeContext, final FlowContext context) {
        Boolean checkResult = nodeContext.get(FlowConstants.NODECTX_PRE_RESULT);
        if (checkResult != null) {
            logger.info("Pre result checked:" + checkResult);
            return checkResult;
        }
        final Object lockObj;
        final FlowContext lockContext  = context;
        synchronized (lockContext) {
            Object contextLockObj = context.get(FlowConstants.CTX_LOCK_PREFIX + nodeContext.getNodeId());
            if (contextLockObj == null) {
                contextLockObj = new Object();
                context.put(FlowConstants.CTX_LOCK_PREFIX + nodeContext.getNodeId(), contextLockObj);
            }
            lockObj = contextLockObj;
        }
        synchronized (lockObj) {
            List<String> preNodes = context.get(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId());
            if (preNodes == null) {
                preNodes = new ArrayList<String>();
                context.put(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId(), preNodes);
            }
            preNodes.add(nodeContext.getPreviousNode().getNodeId());

            FlowNode currentNode = context.getFlow().getNode(nodeContext.getNodeId());
            List<String> configPreNodes = currentNode.getProperty(FlowConstants.PROP_PRENODES);
            logger.info("Pre nodes executed:" + preNodes);
            if (preNodes.size() != configPreNodes.size()) {
                return false;
            }
            for (String s : configPreNodes) {
                if (!preNodes.contains(s)) {
                    throw new FlowException("node info inconsistent, config:" + configPreNodes + ", runtime:" + preNodes);
                }
            }
            logger.info("All pre nodes finished");
            context.remove(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId());
            return true;

        }
    }

}
