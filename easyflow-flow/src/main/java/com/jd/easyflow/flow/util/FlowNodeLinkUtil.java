package com.jd.easyflow.flow.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;
import com.jd.easyflow.flow.model.post.EventPostHandler;
import com.jd.easyflow.flow.model.post.FixedNodePostHandler;
import com.jd.easyflow.flow.model.pre.NodePrePropertyGetter;

/**
 * @author liyuliang5
 */
public class FlowNodeLinkUtil {

    private static final Logger logger = LoggerFactory.getLogger(FlowNodeLinkUtil.class);

    private static final String IDX_VAR_PREFIX = "$";

    private static final String NODE_PROP_NEXT_NODES_CACHE = "_$nextNodesCache";
    private static final String NODE_PROP_PREVIOUS_NODES_CACHE = "_$previousNodesCache";
    private static final String NODE_PROP_PRE_CHECK_TYPE_CACHE = "_$preCheckTypeCache";
    private static final String NODE_PROP_PRE_CHECK_NODES_CACHE = "_$preCheckNodesCache";

    private static final String NODE_PROP_PRE_CHECK_TYPE = "preCheckType";

    static final String FLOW_PROP_REACHABLE_MAP = "_$reachableMap";

    public static final String NODE_ID_ALL = "$*";
    public static final String NODE_ID_UNKNOWN = "$?";
    public static final String NODE_PRE_CHECK_TYPE_UNKNOWN = "?";

    private static List<String> EMPTY_LIST = new ArrayList<String>(0);

    /**
     * 
     * @param nodeId
     * @param flow
     * @return resultList. may null or contains null! return null if uncomputable.
     *         contains null as uncomputable.
     */
    public static List<String> getNextNodes(String nodeId, Flow flow) {
        FlowNode node = flow.getNode(nodeId);
        return getNextNodes(node, flow);
    }

    /**
     * 
     * @param node
     * @param flow
     * @return resultList. may return $? as unknown.
     */
    public static List<String> getNextNodes(FlowNode node, Flow flow) {
        List<String> nextNodeIds = node.getProperty(NODE_PROP_NEXT_NODES_CACHE);
        if (nextNodeIds != null) {
            return nextNodeIds;
        }
        nextNodeIds = node.getProperty(FlowConstants.NODE_PROP_NEXT_NODES);
        if (nextNodeIds == null) {
            if (node instanceof NodeImpl) {
                NodeImpl nodeImpl = (NodeImpl) node;
                NodePostHandler nodePostHandler = nodeImpl.getPostHandler();
                nextNodeIds = getNextNodeIds(nodePostHandler, nodeImpl, flow);
            } else {
                nextNodeIds = Arrays.asList(NODE_ID_UNKNOWN);
            }
        }
        if (nextNodeIds.size() > 1) {
            Set<String> nextNodeIdsSet = new LinkedHashSet<String>();
            nextNodeIdsSet.addAll(nextNodeIds);
            nextNodeIds = new ArrayList<String>(nextNodeIdsSet);
        }
        if (nextNodeIds.contains(NODE_ID_UNKNOWN)) {
            if (logger.isInfoEnabled()) {
                logger.info(flow.getId() + ":" + node.getId() + " next nodes contains unknown");
            }
        }
        node.setProperty(NODE_PROP_NEXT_NODES_CACHE, nextNodeIds);
        return nextNodeIds;
    }

    private static List<String> getNextNodeIds(NodePostHandler nodePostHandler, FlowNode node, Flow flow) {
        if (nodePostHandler == null) {
            return EMPTY_LIST;
        } else if (nodePostHandler instanceof FixedNodePostHandler) {
            FixedNodePostHandler fixed = (FixedNodePostHandler) nodePostHandler;
            return parseTo(fixed.getTo(), node.getId(), flow);
        } else if (nodePostHandler instanceof ConditionalNodePostHandler) {
            List<String> nextNodeIds = new ArrayList<String>();
            ConditionalNodePostHandler conditional = (ConditionalNodePostHandler) nodePostHandler;
            List<Map<String, Object>> branchList = conditional.getBranchList();
            for (Map<String, Object> branch : branchList) {
                Object next = branch.get("to");
                List<String> toList = parseTo(next, node.getId(), flow);
                nextNodeIds.addAll(toList);
            }
            if (conditional.getDefaultBranch() != null) {
                List<String> defaultTo = parseTo(conditional.getDefaultBranch(), node.getId(), flow);
                nextNodeIds.addAll(defaultTo);
            }
            return nextNodeIds;
        } else if (nodePostHandler instanceof EventPostHandler) {
            Map<String, NodePostHandler> handlerMap = node
                    .getProperty(FlowConstants.PROP_RUNTIME_EVENT_POST_HANDLER_MAP);
            if (handlerMap == null) {
                return EMPTY_LIST;
            }
            List<String> nextNodeIds = new ArrayList<String>();
            for (NodePostHandler handler : handlerMap.values()) {
                List<String> list = getNextNodeIds(handler, node, flow);
                nextNodeIds.addAll(list);
            }
            return nextNodeIds;
        } else {
            return Arrays.asList(NODE_ID_UNKNOWN);
        }
    }

    public static List<String> getPreviousNodes(String nodeId, Flow flow) {
        FlowNode node = flow.getNode(nodeId);
        return getPreviousNodeIds(node, flow);
    }

    public static List<String> getPreviousNodeIds(FlowNode node, Flow flow) {
        List<String> previousNodeIds = node.getProperty(NODE_PROP_PREVIOUS_NODES_CACHE);
        if (previousNodeIds != null) {
            return previousNodeIds;
        }
        String nodeId = node.getId();
        previousNodeIds = new ArrayList<String>();
        boolean containsUnknown = false;
        for (FlowNode flowNode : flow.getNodeList()) {
            List<String> nextNodeIds = getNextNodes(flowNode, flow);
            for (String nextNode : nextNodeIds) {
                if (NODE_ID_UNKNOWN.equals(nextNode)) {
                    containsUnknown = true;
                } else if (nodeId.equals(nextNode) || NODE_ID_ALL.equals(nextNode)) {
                    previousNodeIds.add(flowNode.getId());
                    break;
                }
            }
        }
        if (containsUnknown) {
            previousNodeIds.add(NODE_ID_UNKNOWN);
        }
        node.setProperty(NODE_PROP_PREVIOUS_NODES_CACHE, previousNodeIds);
        return previousNodeIds;
    }

    public static String getPreCheckType(String nodeId, Flow flow) {
        FlowNode node = flow.getNode(nodeId);
        return getPreCheckType(node, flow);
    }

    public static String getPreCheckType(FlowNode node, Flow flow) {
        String preCheckType = node.getProperty(NODE_PROP_PRE_CHECK_TYPE_CACHE);
        if (preCheckType != null) {
            return preCheckType;
        }

        preCheckType = node.getProperty(NODE_PROP_PRE_CHECK_TYPE);
        if (preCheckType == null) {
            if (node instanceof NodeImpl) {
                NodeImpl nodeImpl = (NodeImpl) node;
                NodePreHandler nodePreHandler = nodeImpl.getPreHandler();
                if (nodePreHandler == null) {
                    preCheckType = null;
                } else if (nodePreHandler instanceof NodePrePropertyGetter) {
                    preCheckType = ((NodePrePropertyGetter) nodePreHandler).getCheckType();
                } else {
                    preCheckType = NODE_PRE_CHECK_TYPE_UNKNOWN;
                }
            } else {
                preCheckType = NODE_PRE_CHECK_TYPE_UNKNOWN;
            }
        }
        node.setProperty(NODE_PROP_PRE_CHECK_TYPE_CACHE, preCheckType);
        return preCheckType;
    }

    public static List<String> getPreCheckNodes(String nodeId, Flow flow) {
        FlowNode node = flow.getNode(nodeId);
        return getPreCheckNodes(node, flow);
    }
    
    public static List<String> getPreCheckNodes(FlowNode node, Flow flow) {
        List<String> preNodes = node.getProperty(NODE_PROP_PRE_CHECK_NODES_CACHE);
        if (preNodes != null) {
            return preNodes;
        }
        preNodes = node.getProperty(FlowConstants.PROP_PRENODES);
        if (preNodes == null) {
            preNodes = new ArrayList<String>();
            if (node instanceof NodeImpl) {
                NodeImpl nodeImpl = (NodeImpl) node;
                NodePreHandler nodePreHandler = nodeImpl.getPreHandler();
                if (nodePreHandler == null) {
                    // NOOP
                } else if (nodePreHandler instanceof NodePrePropertyGetter) {
                    preNodes = ((NodePrePropertyGetter) nodePreHandler).getPreNodes();
                } else {
                    preNodes = Arrays.asList(NODE_ID_UNKNOWN);
                }
            } else {
                preNodes = Arrays.asList(NODE_ID_UNKNOWN);
            }
        }
        node.setProperty(NODE_PROP_PRE_CHECK_NODES_CACHE, preNodes);
        return preNodes;
    }

    private static List<String> parseTo(Object to, String nodeId, Flow flow) {
        List<String> result = new ArrayList<String>();
        // String type
        if (to instanceof String) {
            String toStr = (String) to;
            if (!toStr.startsWith(IDX_VAR_PREFIX)) {
                result.add(toStr);
            } else {
                toStr = parseIndexVar(toStr, nodeId, flow);
                result.add(toStr);
            }
        } else if (to instanceof Integer) {
            int toIdx = (Integer) to;
            String toStr = flow.getNodeList().get(toIdx).getId();
            result.add(toStr);
            // List type
        } else if (to instanceof List) {
            List<Object> toList = (List) to;
            List<NodeContext> toResult = new ArrayList<>(toList.size());
            for (Object toObj : toList) {
                List<String> nodes = parseTo(toObj, nodeId, flow);
                result.addAll(nodes);
            }
        } else if (to instanceof Map) {
            Map<String, Object> toMap = (Map<String, Object>) to;
            Object exp = toMap.get("exp");
            if (exp != null) {
                result.add(NODE_ID_UNKNOWN);
            } else {
                Object node = toMap.get("node");
                if (node != null) {
                    List<String> nodes = parseTo(node, nodeId, flow);
                    result.addAll(nodes);
                }
            }
            
        } else {
            result.add(NODE_ID_UNKNOWN);
        }
        return result;
    }

    private static String parseIndexVar(String var, String nodeId, Flow flow) {
        int index = -1;
        switch (var) {
        case "$first": {
            index = 0;
            break;
        }
        case "$last": {
            index = flow.getNodeList().size() - 1;
            break;
        }
        case "$previous": {
            index = flow.getNodeIndex(nodeId) - 1;
            break;
        }
        case "$next": {
            index = flow.getNodeIndex(nodeId) + 1;
            break;
        }
        default: {
            return NODE_ID_UNKNOWN;
        }
        }
        return flow.getNodeList().get(index).getId();
    }

    public static boolean isReachable(String sourceNodeId, String targetNodeId, Flow flow) {
        Map<String, Boolean> reachableMap = flow.getProperty(FLOW_PROP_REACHABLE_MAP);
        if (reachableMap == null) {
            reachableMap = new ConcurrentHashMap<String, Boolean>();
            flow.setProperty(FLOW_PROP_REACHABLE_MAP, reachableMap);
        }
        String key = buildKey(sourceNodeId, targetNodeId, flow);
        Boolean result = reachableMap.get(key);
        if (result != null) {
            return result;
        }
        Set<String> set = new HashSet<>();
        result = computeReachable(set, sourceNodeId, targetNodeId, flow);
        reachableMap.put(key, result);
        return result;

    }

    private static boolean computeReachable(Set<String> nodeSet, String sourceNodeId, String targetNodeId, Flow flow) {
        boolean notContains = nodeSet.add(sourceNodeId);
        if (!notContains) {
            return false;
        }
        FlowNode sourceNode = flow.getNode(sourceNodeId);
        List<String> nextNodeIds = getNextNodes(sourceNode, flow);
        for (String nextNodeId : nextNodeIds) {
            if (NODE_ID_UNKNOWN.equals(nextNodeId)) {
                logger.warn("nextNodeId contains unknown to ignore, flow:" + flow.getId() + " sourceNodeId:"
                        + sourceNodeId + " targetNodeId:" + targetNodeId);
            }
            if (nextNodeId.equals(targetNodeId) || NODE_ID_ALL.equals(nextNodeId)) {
                return true;
            }
            boolean result = computeReachable(nodeSet, nextNodeId, targetNodeId, flow);
            if (result) {
                return true;
            }
        }
        return false;

    }

    private static String buildKey(String sourceNodeId, String targetNodeId, Flow flow) {
        return sourceNodeId + "$" + targetNodeId;
    }

}
