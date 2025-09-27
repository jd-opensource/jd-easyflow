package com.jd.easyflow.process.client.runtime;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.jd.easyflow.process.client.runtime.core.Node;
import com.jd.easyflow.process.client.runtime.core.NodeContext;
import com.jd.easyflow.process.client.util.Pair;

/**
 * @author liyuliang5
 *
 */
public class StdNodeContext implements NodeContext {
    
    private Node node;
        
    private String nodeInstanceNo;

    private String previousNodeId;
    
    private String nodeId;
    
    private String[] nextNodeIds;
    
    private String[] nextNodeInstanceNos;
    
    private String eventId;
    
    private Date executionStartTime;
    
    private Map<String, Object> extData;

    private List<String> configPreNodeIds;

    private String preCheckType;
    
    private Boolean preResult = true;
    
    private Map<String, Object> processProperties;

    private Map<String, Object> processParamProperties;
    
    private Map<String, Object> dataMap;
    
    @JsonIgnore
    private StdProcessContext stdProcessContext;
    
    @JsonIgnore
    private Object engineNodeContext;
    
    private Object actionResult;
    
    public Node getNode() {
        return node;
    }
    public void setNode(Node node) {
        this.node = node;
    }
    public String getNodeInstanceNo() {
        return nodeInstanceNo;
    }
    public void setNodeInstanceNo(String nodeInstanceNo) {
        this.nodeInstanceNo = nodeInstanceNo;
    }
    public String getPreviousNodeId() {
        return previousNodeId;
    }
    public void setPreviousNodeId(String previousNodeId) {
        this.previousNodeId = previousNodeId;
    }
    public String getNodeId() {
        return nodeId;
    }
    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }
    public String[] getNextNodeIds() {
        return nextNodeIds;
    }
    public void setNextNodeIds(String[] nextNodeIds) {
        this.nextNodeIds = nextNodeIds;
    }
    public String[] getNextNodeInstanceNos() {
        return nextNodeInstanceNos;
    }
    public void setNextNodeInstanceNos(String[] nextNodeInstanceNos) {
        this.nextNodeInstanceNos = nextNodeInstanceNos;
    }
    public String getEventId() {
        return eventId;
    }
    public void setEventId(String eventId) {
        this.eventId = eventId;
    }
    public Date getExecutionStartTime() {
        return executionStartTime;
    }
    public void setExecutionStartTime(Date executionStartTime) {
        this.executionStartTime = executionStartTime;
    }
    public Map<String, Object> getExtData() {
        return extData;
    }
    public void setExtData(Map<String, Object> extData) {
        this.extData = extData;
    }
    public List<String> getConfigPreNodeIds() {
        return configPreNodeIds;
    }
    public void setConfigPreNodeIds(List<String> configPreNodeIds) {
        this.configPreNodeIds = configPreNodeIds;
    }
    
    public Map<String, Object> getProcessProperties() {
        return processProperties;
    }
    public void setProcessProperties(Map<String, Object> processProperties) {
        this.processProperties = processProperties;
    }
    public Map<String, Object> getProcessParamProperties() {
        return processParamProperties;
    }
    public void setProcessParamProperties(Map<String, Object> processParamProperties) {
        this.processParamProperties = processParamProperties;
    }
    public String getPreCheckType() {
        return preCheckType;
    }
    public void setPreCheckType(String preCheckType) {
        this.preCheckType = preCheckType;
    }
    public Boolean getPreResult() {
        return preResult;
    }
    public void setPreResult(Boolean preResult) {
        this.preResult = preResult;
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
        if (value == null) {
            dataMap.remove(key);
        } else {
            dataMap.put(key, value);
        }
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
    public StdProcessContext getStdProcessContext() {
        return stdProcessContext;
    }
    public void setStdProcessContext(StdProcessContext stdProcessContext) {
        this.stdProcessContext = stdProcessContext;
    }
  
    public Map<String, String> getVariables() {
        return this.stdProcessContext.getNodeVariableGetter().apply(this);
    }
    
    public void setVariables(Map<String, String> variables) {
        this.stdProcessContext.getNodeVariableSetter().accept(Pair.of(this, variables));
    }
    
    public void putVariable(String key, String value) {
        Map<String, String> variables = getVariables();
        if (variables == null) {
            synchronized (stdProcessContext.getLock()) {
                variables = getVariables();
                if (variables == null) {
                    variables = new ConcurrentHashMap<String, String>();
                    this.setVariables(variables);
                }
            }
        }
        variables.put(key, value);
    }
    
    public String getVariable(String key) {
        Map<String, String> variables = this.getVariables();
        if (variables == null) {
            return null;
        }
        return variables.get(key);
    }
    
    public <T>T getEngineNodeContext() {
        return (T) engineNodeContext;
    }
    public void setEngineNodeContext(Object engineNodeContext) {
        this.engineNodeContext = engineNodeContext;
    }
    
    public Object getActionResult() {
        return actionResult;
    }
    public void setActionResult(Object actionResult) {
        this.actionResult = actionResult;
    }
    @Override
    public String toString() {
        return "StdNodeContext [nodeInstanceNo=" + nodeInstanceNo + ", previousNodeId=" + previousNodeId + ", nodeId="
                + nodeId + ", nextNodeIds=" + Arrays.toString(nextNodeIds) + ", nextNodeInstanceNos="
                + Arrays.toString(nextNodeInstanceNos) + ", eventId=" + eventId + ", executionStartTime="
                + executionStartTime + ", extData=" + extData + ", configPreNodeIds=" + configPreNodeIds
                + ", preCheckType=" + preCheckType + ", preResult=" + preResult + ", processProperties="
                + processProperties + ", processParamProperties=" + processParamProperties + ", dataMap=" + dataMap
                + "]";
    }
    
}
