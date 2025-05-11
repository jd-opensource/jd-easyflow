package com.jd.easyflow.flow.ext.check.impl;

import java.util.List;

import com.jd.easyflow.flow.ext.check.CheckErrorItem;
import com.jd.easyflow.flow.ext.check.CheckParam;
import com.jd.easyflow.flow.ext.check.CheckResult;
import com.jd.easyflow.flow.ext.check.FlowChecker;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.util.FlowNodeLinkUtil;

/**
 * @author liyuliang5
 */
public class FlowNodeLinkChecker implements FlowChecker {

    public static final String ERROR_TYPE_NODE_ISOLATED = "NODE_ISOLATED";
    public static final String ERROR_TYPE_NEXT_NODES_NOT_EXISTS = "NEXT_NODES_NOT_EXISTS";
    public static final String ERROR_TYPE_PRE_CHECK_NODES_NOT_EXISTS = "PRE_CHECK_NODES_NOT_EXISTS";
    public static final String ERROR_TYPE_NON_START_NODE_NO_PREVIOUS = "NON_START_NODE_NO_PREVIOUS";
    public static final String ERROR_TYPE_NON_END_NODE_NO_NEXT = "NON_END_NODE_NO_NEXT";
    
    public CheckResult check(CheckParam param) {
        Flow flow = param.getFlow();
        FlowNodeLinkCheckConfig config = (FlowNodeLinkCheckConfig) param.getConfig();
        CheckResult result = new CheckResult();
        if (flow.getNodeList() == null) {
            return result;
        }
        for (FlowNode node : flow.getNodeList()) {
            List<String> previousNodeIds = FlowNodeLinkUtil.getPreviousNodeIds(node, flow);
            List<String> nextNodeIds = FlowNodeLinkUtil.getNextNodes(node, flow);
            List<String> preCheckNodeIds = FlowNodeLinkUtil.getPreCheckNodes(node, flow);
            // node isolated.
            if (config.isCheckNodeIsolated()) {
                if (previousNodeIds.size() == 0 && nextNodeIds.size() == 0 && flow.getNodeList().size() > 1) {
                    result.addErrorItem(new CheckErrorItem(ERROR_TYPE_NODE_ISOLATED, null, flow.getId(), node.getId(), flow));
                }
            }
            // next nodes none exists.
            if (config.isCheckNextNodesNotExists()) {
                if (nextNodeIds.size() > 0) {
                    for (String nodeId : nextNodeIds) {
                        if (!nodeId.equals(FlowNodeLinkUtil.NODE_ID_UNKNOWN) && flow.getNode(nodeId) == null) {
                            result.addErrorItem(new CheckErrorItem(ERROR_TYPE_NEXT_NODES_NOT_EXISTS, null, flow.getId(), node.getId(), flow));
                        }
                    }
                }
            }
            // pre check nodes none exists.
            if (config.isCheckPreCheckNodesNotExists()) {
                if (preCheckNodeIds.size() > 0) {
                    for (String nodeId : preCheckNodeIds) {
                        if (!nodeId.equals(FlowNodeLinkUtil.NODE_ID_UNKNOWN) && flow.getNode(nodeId) == null) {
                            result.addErrorItem(new CheckErrorItem(ERROR_TYPE_PRE_CHECK_NODES_NOT_EXISTS, null, flow.getId(), node.getId(), flow));
                        }
                    }
                }
            }
            // non-start node has no previous.
            if (config.isCheckNonStartNodeNoPrevious()) {
                if (previousNodeIds.size() == 0 && !arrayContains(flow.getStartNodeIds(), node.getId())) {
                    result.addErrorItem(new CheckErrorItem(ERROR_TYPE_NON_START_NODE_NO_PREVIOUS, null, flow.getId(), node.getId(), flow));
                }
            }
            // non-end node has no next.
            if (config.isCheckNonEndNodeNoNext()) {
                if (nextNodeIds.size() == 0 && ! Boolean.TRUE.equals(node.getProperty("end"))) {
                    result.addErrorItem(new CheckErrorItem(ERROR_TYPE_NON_END_NODE_NO_NEXT, null, flow.getId(), node.getId(), flow));
                }
            }
        }
        return result;
    }

    private static boolean arrayContains(String[] array, String element) {
        if (array == null) {
            return false;
        }
        for (String e : array) {
            if (element.equals(e)) {
                return true;
            }
        }
        return false;
    }
}
