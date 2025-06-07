package com.jd.easyflow.flow.model;

import java.util.Map;

/**
 * @author liyuliang5.
 * Using separate setter to avoid mistake setting.
 * 
 */
public class NodeContextAccessor {

    public static void setNodeId(NodeContext nodeContext, String nodeId) {
        nodeContext.setNodeId(nodeId);
    }
    
    public static void setPreResult(NodeContext nodeContext, Boolean preResult) {
        nodeContext.setPreResult(preResult);
    }
    
    public static void setActionResult(NodeContext nodeContext, Object actionResult) {
        nodeContext.setActionResult(actionResult);
    }
    
    public static void setNextNodeIds(NodeContext nodeContext, String[] nextNodeIds) {
        nodeContext.setNextNodeIds(nextNodeIds);
    }
    
    public static void setNextNodes(NodeContext nodeContext, NodeContext[] nextNodes) {
        nodeContext.setNextNodes(nextNodes);
    }
    
    public static void setPreviousNode(NodeContext nodeContext, NodeContext previousNode) {
        nodeContext.setPreviousNode(previousNode);
    }
    
    public static void setThrowable(NodeContext nodeContext, Throwable t) {
        nodeContext.setThrowable(t);
    }
    
    public static void setDataMap(NodeContext nodeContext, Map<String, Object> dataMap) {
        nodeContext.setDataMap(dataMap);
    }
}
