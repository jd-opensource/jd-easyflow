package com.jd.easyflow.process.client.runtime.core;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author liyuliang5
 *
 */
public class ProcessParam {


    public ProcessParam() {
        // NOOP
    }
    
    public ProcessParam(String processId) {
        this.processId = processId;
    }
    
    public ProcessParam(String processId, Object param) {
        this.processId = processId;
        this.param = param;
    }
    
    public ProcessParam(String processId, String[] nodeIds, Object param) {
        this.processId = processId;
        this.nodeIds = nodeIds;
        this.param = param;
    }
    
    public ProcessParam(String processId, String nodeId, Object param) {
        this.processId = processId;
        this.nodeIds = new String[] {nodeId};
        this.param = param;
    }

    private String processId;

    private String[] nodeIds;
    /**
     * business param.
     */
    private Object param;

    /**
     * common param data.
     */
    private Map<String, Object> dataMap;

    public String getProcessId() {
        return processId;
    }

    public void setProcessId(String processId) {
        this.processId = processId;
    }

    public String[] getNodeIds() {
        return nodeIds;
    }

    public void setNodeIds(String[] nodeIds) {
        this.nodeIds = nodeIds;
    }

    public void setNodeId(String nodeId) {
        if (nodeId == null) {
            return;
        }
        this.nodeIds = new String[] { nodeId };
    }

    public <T> T getParam() {
        return (T) param;
    }

    /**
     * param should be Object[] or List
     * 
     * @param <T>
     * @param index
     * @return
     */
    public <T> T getParam(int index) {
        if (param == null) {
            return null;
        }
        if (param instanceof Object[]) {
            return (T) ((Object[]) param)[index];
        } else if (param instanceof List) {
            return (T) ((List) param).get(index);
        }
        throw new IllegalStateException("Param:" + param + " is not index type");
    }

    /**
     * param should be Map
     * 
     * @param <T>
     * @param key
     * @return
     */
    public <T> T getParam(String key) {
        if (param == null) {
            return null;
        }
        return (T) ((Map<String, Object>) param).get(key);
    }

    public void setParam(Object param) {
        this.param = param;
    }

    public Map<String, Object> getDataMap() {
        return dataMap;
    }

    public void put(String key, Object value) {
        if (dataMap == null) {
            dataMap = new ConcurrentHashMap<>();
        }
        dataMap.put(key, value);
    }

    public <T> T get(String key) {
        if (dataMap == null) {
            return null;
        }
        return (T) dataMap.get(key);
    }

    public void setDataMap(Map<String, Object> dataMap) {
        this.dataMap = dataMap;
    }
    
    
}
