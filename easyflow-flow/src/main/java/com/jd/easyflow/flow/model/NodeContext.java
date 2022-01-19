package com.jd.easyflow.flow.model;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 
 * @author liyuliang5
 *
 */
public class NodeContext {

    public NodeContext() {
    }

    public NodeContext(String nodeId) {
        this.nodeId = nodeId;
    }

    private String nodeId;

    private NodeContext previousNode;

    private NodeContext[] nextNodes;
    
    private boolean preResult;

    private Object actionResult;
    
    private Throwable throwable;

    /**
     * 通用数据Map;
     */
    private Map<String, Object> dataMap;

    public Object getActionResult() {
        return actionResult;
    }

    public void setActionResult(Object actionResult) {
        this.actionResult = actionResult;
    }

    public Map<String, Object> getDataMap() {
        return dataMap;
    }

    public void setDataMap(Map<String, Object> dataMap) {
        this.dataMap = dataMap;
    }

    public void put(String key, Object value) {
        if (dataMap == null) {
            dataMap = new ConcurrentHashMap<String, Object>();
        }
        dataMap.put(key, value);
    }

    public <T> T get(String key) {
        if (dataMap == null) {
            return null;
        }
        return (T) dataMap.get(key);
    }

    public void remove(String key) {
        dataMap.remove(key);
    }

    public void setNextNodeIds(String[] nextNodeIds) {
        if (nextNodeIds == null) {
            this.nextNodes = null;
            return;
        }
        nextNodes = new NodeContext[nextNodeIds.length];
        for (int i = 0; i < nextNodeIds.length; i++) {
            NodeContext nextNode = new NodeContext(nextNodeIds[i]);
            nextNode.setPreviousNode(this);
            nextNodes[i] = nextNode;
        }
    }

    public void setNextNodes(NodeContext[] nextNodes) {
        this.nextNodes = nextNodes;
        if (nextNodes != null) {
            for (int i = 0; i < nextNodes.length; i++) {
                nextNodes[i].setPreviousNode(this);
                ;
            }
        }
    }

    public NodeContext[] getNextNodes() {
        return nextNodes;
    }

    public String getNodeId() {
        return nodeId;
    }

    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }

    public NodeContext getPreviousNode() {
        return previousNode;
    }

    public void setPreviousNode(NodeContext previousNode) {
        this.previousNode = previousNode;
    }

    public Throwable getThrowable() {
        return throwable;
    }

    public void setThrowable(Throwable throwable) {
        this.throwable = throwable;
    }

    public boolean isPreResult() {
        return preResult;
    }

    public void setPreResult(boolean preResult) {
        this.preResult = preResult;
    }
    
}
