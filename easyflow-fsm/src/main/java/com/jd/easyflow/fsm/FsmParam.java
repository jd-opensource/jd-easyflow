package com.jd.easyflow.fsm;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * 
* 
* Fsm Param.
* CREATE: No required field.
* EXECUTE:EventId is necessary.
* 
* @author liyuliang5
* @version 1.0
* @since 1.0
 */
public class FsmParam {
    
    public enum OpType {
        /**Create Fsm Instance*/
        CREATE, 
        /**Execute Fsm*/
        EXECUTE
    }
    
    private OpType opType = OpType.EXECUTE;
    
    private String fsmId;
    
    private String bizType;
    
    private String instanceId;
    
    private Object instance;
    
    private String currentStateId;
    
    private String eventId;
    @JsonIgnore
    private FsmContext context;
    // business param
    private Object param;
    
    private Map<String, Object> dataMap;

    public String getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(String instanceId) {
        this.instanceId = instanceId;
    }

    public String getEventId() {
        return eventId;
    }

    public void setEventId(String eventId) {
        this.eventId = eventId;
    }

    public FsmContext getContext() {
        return context;
    }

    public void setContext(FsmContext context) {
        this.context = context;
    }

    public Object getInstance() {
        return instance;
    }

    public void setInstance(Object instance) {
        this.instance = instance;
    }

    public String getCurrentStateId() {
        return currentStateId;
    }

    public void setCurrentStateId(String currentStateId) {
        this.currentStateId = currentStateId;
    }
    
    public <T>T getParam() {
        return (T) param;
    }

    public void setParam(Object param) {
        this.param = param;
    }

    public OpType getOpType() {
        return opType;
    }

    public void setOpType(OpType opType) {
        this.opType = opType;
    }

    public String getFsmId() {
        return fsmId;
    }

    public void setFsmId(String fsmId) {
        this.fsmId = fsmId;
    }

    public String getBizType() {
        return bizType;
    }

    public void setBizType(String bizType) {
        this.bizType = bizType;
    }
    

    public Map<String, Object> getDataMap() {
        return dataMap;
    }

    public void seDataMap(Map<String, Object> dataMap) {
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
    
    public void putContextData(String key, Object value) {
        if (context == null) {
            context = new FsmContext();
        }
        context.putData(key, value);
    }
    
    public void setBizContext(Object bizContext) {
        if (context == null) {
            context = new FsmContext();
        }
        context.setContext(bizContext);
    }
    
}
