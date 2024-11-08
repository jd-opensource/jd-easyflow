package com.jd.easyflow.process.client.runtime;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.client.runtime.core.ProcessContext;
import com.jd.easyflow.process.client.util.Pair;

/**
 * @author liyuliang5
 * 
 */
public class StdProcessContext implements ProcessContext {
    
    private StdProcess process;

    private String instanceNo;

    private Map<String, Object> processProperties;

    private Map<String, Object> processParamProperties;
    
    private List<String> startNodeIds;
    
    private Object lock = new Object();
    
    private String lockRequestId;
    
    private ProcessCache cache = new ProcessCache();

    private Consumer<Map<String, String>> variableSetter;
    
    private Supplier<Map<String, String>> variableGetter;
    
    private Consumer<Pair<StdNodeContext, Map<String, String>>> nodeVariableSetter;
    
    private Function<StdNodeContext, Map<String, String>> nodeVariableGetter;
    
    private Function<String, StdNode> nodeFunction;
    
    private Function<ProcessInstanceDTO, List<String>> startNodesFunction;

    private Function<Object, Object> eventTriggerFunction;
    
    private Boolean start;
    
    private Boolean end;
    
    private boolean checkStartNode = true;
    
    private String bizLockKey;

    private String bizLockRequestId;

    private String instanceLockKey;
    private String instanceLockRequestId;

    private String processType;

    private String bizNo;
    
    private String opType;
    
    /**
     * interrupted
     */
    private volatile boolean interrupted = false;
    
    private int nodeStartEventPolicy;
    
    /**
     * Common data map.
     */
    private Map<String, Object> data = new ConcurrentHashMap<String, Object>();
    
    @JsonIgnore
    private Object engineProcessContext;
    
    private boolean subProcess;
    
    public void put(String key, Object value) {
        if (value == null) {
            data.remove(key);
        } else {
            data.put(key, value);
        }
    }

    public <T> T get(String key) {
        return (T) data.get(key);
    }
    
    public <T>T getProcessProperty(String key) {
        if (processProperties == null) {
            return null;
        }
        return (T) processProperties.get(key);
    }

    public StdProcess getProcess() {
        return process;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
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

    public List<String> getStartNodeIds() {
        return startNodeIds;
    }

    public void setStartNodeIds(List<String> startNodeIds) {
        this.startNodeIds = startNodeIds;
    }

    public Object getLock() {
        return lock;
    }

    public void setLock(Object lock) {
        this.lock = lock;
    }

    public String getLockRequestId() {
        return lockRequestId;
    }

    public void setLockRequestId(String lockRequestId) {
        this.lockRequestId = lockRequestId;
    }

    public ProcessCache getCache() {
        return cache;
    }

    public void setCache(ProcessCache cache) {
        this.cache = cache;
    }

    public Consumer<Map<String, String>> getVariableSetter() {
        return variableSetter;
    }

    public void setVariableSetter(Consumer<Map<String, String>> variableSetter) {
        this.variableSetter = variableSetter;
    }

    public Supplier<Map<String, String>> getVariableGetter() {
        return variableGetter;
    }

    public void setVariableGetter(Supplier<Map<String, String>> variableGetter) {
        this.variableGetter = variableGetter;
    }

    public Function<String, StdNode> getNodeFunction() {
        return nodeFunction;
    }

    public void setNodeFunction(Function<String, StdNode> nodeFunction) {
        this.nodeFunction = nodeFunction;
    }

    public Function<ProcessInstanceDTO, List<String>> getStartNodesFunction() {
        return startNodesFunction;
    }

    public void setStartNodesFunction(Function<ProcessInstanceDTO, List<String>> startNodesFunction) {
        this.startNodesFunction = startNodesFunction;
    }

    public Boolean getStart() {
        return start;
    }

    public void setStart(Boolean start) {
        this.start = start;
    }

    public Boolean getEnd() {
        return end;
    }

    public void setEnd(Boolean end) {
        this.end = end;
    }

    public boolean isCheckStartNode() {
        return checkStartNode;
    }

    public void setCheckStartNode(boolean checkStartNode) {
        this.checkStartNode = checkStartNode;
    }

    public String getBizLockKey() {
        return bizLockKey;
    }

    public void setBizLockKey(String bizLockKey) {
        this.bizLockKey = bizLockKey;
    }

    public String getBizLockRequestId() {
        return bizLockRequestId;
    }

    public void setBizLockRequestId(String bizLockRequestId) {
        this.bizLockRequestId = bizLockRequestId;
    }

    public String getInstanceLockKey() {
        return instanceLockKey;
    }

    public void setInstanceLockKey(String instanceLockKey) {
        this.instanceLockKey = instanceLockKey;
    }

    public String getInstanceLockRequestId() {
        return instanceLockRequestId;
    }

    public void setInstanceLockRequestId(String instanceLockRequestId) {
        this.instanceLockRequestId = instanceLockRequestId;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getOpType() {
        return opType;
    }

    public void setOpType(String opType) {
        this.opType = opType;
    }

    public String getProcessType() {
        return processType;
    }

    public void setProcessType(String processType) {
        this.processType = processType;
    }

    public void setProcess(StdProcess process) {
        this.process = process;
    }

    public Function<Object, Object> getEventTriggerFunction() {
        return eventTriggerFunction;
    }

    public void setEventTriggerFunction(Function<Object, Object> eventTriggerFunction) {
        this.eventTriggerFunction = eventTriggerFunction;
    }

    public boolean isInterrupted() {
        return interrupted;
    }

    public void setInterrupted() {
        this.interrupted = true;
    }

    public Map<String, Object> getData() {
        return data;
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }

    public int getNodeStartEventPolicy() {
        return nodeStartEventPolicy;
    }

    public void setNodeStartEventPolicy(int nodeStartEventPolicy) {
        this.nodeStartEventPolicy = nodeStartEventPolicy;
    }
    
    public Map<String, String> getVariables() {
        return this.variableGetter.get();
    }
    
    public void setVariables(Map<String, String> variables) {
        this.variableSetter.accept(variables);
    }
    
    public void putVariable(String key, String value) {
        Map<String, String> variables = this.variableGetter.get();
        if (variables == null) {
            synchronized (this.getLock()) {
                variables = this.variableGetter.get();
                if (variables == null) {
                    variables = new ConcurrentHashMap<String, String>();
                    this.variableSetter.accept(variables);
                }
            }
        }
        variables.put(key, value);
    }
    
    public String getVariable(String key) {
        Map<String, String> variables = this.variableGetter.get();
        if (variables == null) {
            return null;
        }
        return variables.get(key);
    }

    public Consumer<Pair<StdNodeContext, Map<String, String>>> getNodeVariableSetter() {
        return nodeVariableSetter;
    }

    public void setNodeVariableSetter(Consumer<Pair<StdNodeContext, Map<String, String>>> nodeVariableSetter) {
        this.nodeVariableSetter = nodeVariableSetter;
    }

    public Function<StdNodeContext, Map<String, String>> getNodeVariableGetter() {
        return nodeVariableGetter;
    }

    public void setNodeVariableGetter(Function<StdNodeContext, Map<String, String>> nodeVariableGetter) {
        this.nodeVariableGetter = nodeVariableGetter;
    }

    public <T>T getEngineProcessContext() {
        return (T) engineProcessContext;
    }

    public void setEngineProcessContext(Object engineProcessContext) {
        this.engineProcessContext = engineProcessContext;
    }

    public boolean isSubProcess() {
        return subProcess;
    }

    public void setSubProcess(boolean subProcess) {
        this.subProcess = subProcess;
    }

    @Override
    public String toString() {
        return "StdProcessContext [instanceNo=" + instanceNo + ", processProperties=" + processProperties
                + ", processParamProperties=" + processParamProperties + ", startNodeIds=" + startNodeIds
                + ", lockRequestId=" + lockRequestId + ", cache=" + cache + ", start=" + start + ", end=" + end
                + ", checkStartNode=" + checkStartNode + ", bizLockKey=" + bizLockKey + ", bizLockRequestId="
                + bizLockRequestId + ", instanceLockKey=" + instanceLockKey + ", instanceLockRequestId="
                + instanceLockRequestId + ", processType=" + processType + ", bizNo=" + bizNo + ", opType=" + opType
                + ", interrupted=" + interrupted + ", nodeStartEventPolicy=" + nodeStartEventPolicy + ", data=" + data
                + ", subProcess=" + subProcess + "]";
    }
    
    

}
