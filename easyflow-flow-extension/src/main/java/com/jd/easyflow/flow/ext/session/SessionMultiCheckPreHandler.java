package com.jd.easyflow.flow.ext.session;

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
 * Session level multiple nodes check pre-handler.
 * 
 * @author liyuliang5
 *
 */
public class SessionMultiCheckPreHandler implements NodePreHandler {

    private static final Logger logger = LoggerFactory.getLogger(SessionMultiCheckPreHandler.class);

    private List<String> preNodes;

    public SessionMultiCheckPreHandler() {

    }

    public SessionMultiCheckPreHandler(List<String> preNodes) {
        this.preNodes = preNodes;
    }

    /**
     * Judge all pre nodes finished.
     * 
     */
    @Override
    public boolean preHandle(NodeContext nodeContext, final FlowContext context) {
        if (nodeContext.getPreviousNode() == null) {
            return false;
        }
        Boolean checkResult = nodeContext.get(FlowConstants.NODECTX_PRE_RESULT);
        if (checkResult != null) {
            logger.info("Pre result checked:" + checkResult);
            return checkResult;
        }
        final Object lockObj;
        final FlowSession session = context.get(FlowSessionConstants.CONTEXT_SESSION_KEY);
        synchronized (session) {
            Object sessionLockObj = session.get(FlowConstants.CTX_LOCK_PREFIX + nodeContext.getNodeId());
            if (sessionLockObj == null) {
                sessionLockObj = new Object();
                session.put(FlowConstants.CTX_LOCK_PREFIX + nodeContext.getNodeId(), sessionLockObj);
            }
            lockObj = sessionLockObj;
        }
        synchronized (lockObj) {
            List<String> preNodes = session.get(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId());
            if (preNodes == null) {
                preNodes = new ArrayList<String>();
                session.put(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId(), preNodes);
            }
            preNodes.add(nodeContext.getPreviousNode().getNodeId());

            FlowNode currentNode = context.getFlow().getNode(nodeContext.getNodeId());
            List<String> configPreNodes = this.preNodes != null ? this.preNodes
                    : currentNode.getProperty(FlowConstants.PROP_PRENODES);
            logger.info("Pre nodes executed:" + preNodes);
            if (preNodes.size() != configPreNodes.size()) {
                return false;
            }
            for (String s : configPreNodes) {
                if (!preNodes.contains(s)) {
                    throw new FlowException(
                            "node info inconsistent, config:" + configPreNodes + ", runtime:" + preNodes);
                }
            }
            logger.info("All pre nodes finished");
            session.remove(FlowConstants.CTX_PRE_NODES_PREFIX + nodeContext.getNodeId());
            return true;

        }
    }

}
