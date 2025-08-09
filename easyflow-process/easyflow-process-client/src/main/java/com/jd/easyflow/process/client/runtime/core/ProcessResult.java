package com.jd.easyflow.process.client.runtime.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.jd.easyflow.process.client.runtime.StdProcess;

/**
 * @author liyuliang5
 *
 */
public class ProcessResult {


    @JsonIgnore
    private StdProcess context;

    private Object result;
    
    private String processInstanceNo;
    
    private Map<String, Object> dataMap;

    public StdProcess getContext() {
        return context;
    }

    public void setContext(StdProcess context) {
        this.context = context;
    }

    public synchronized <T>T getResult() {
        return (T) result;
    }

    public synchronized void setResult(Object result) {
        this.result = result;
    }

    /**
     * 
     * Put result. putResult or addResult should use only one.
     *
     * @param key
     * @param value
     */
    public synchronized void putResult(String key, Object value) {
        if (result == null) {
            result = new ConcurrentHashMap<>();
        }
        ((Map<String, Object>) result).put(key, value);
    }
    /**
     * 
     * Add result. putResult or addResult should use only one.
     *
     * @param value
     */
    public synchronized void addResult(Object value) {
        if (result == null) {
            result = new ArrayList<Object>();
        }
        ((List<Object>) result).add(value);
    }
    
    public synchronized <T>T getResult(String key) {
        if (result == null) {
            return null;
        }
        return (T) ((Map<String, Object>) result).get(key);
    }
    
    public synchronized <T>T getResult(int index) {
        if (result == null) {
            return null;
        }
        return (T) ((List<Object>) result).get(index);
    }

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public Map<String, Object> getDataMap() {
        return dataMap;
    }

    public void setDataMap(Map<String, Object> dataMap) {
        this.dataMap = dataMap;
    }
    
    
}
