package com.jd.easyflow.flow.engine;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowResult {

    @JsonIgnore
    private FlowContext context;

    private Object result;
    
    /**
     * common result data.
     */
    private Map<String, Object> dataMap;

    public FlowContext getContext() {
        return context;
    }

    public void setContext(FlowContext context) {
        this.context = context;
    }

    public synchronized <T> T getResult() {
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
        if (value == null) {
            ((Map<String, Object>) result).remove(key);
        } else {
            ((Map<String, Object>) result).put(key, value);
        }
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

    public synchronized <T> T getResult(String key) {
        if (result == null) {
            return null;
        }
        return (T) ((Map<String, Object>) result).get(key);
    }

    public synchronized <T> T getResult(int index) {
        if (result == null) {
            return null;
        }
        return (T) ((List<Object>) result).get(index);
    }
    
    public Map<String, Object> getDataMap() {
        return dataMap;
    }

    public void seDataMap(Map<String, Object> dataMap) {
        this.dataMap = dataMap;
    }

    public void put(String key, Object value) {
        if (dataMap == null) {
            dataMap = new ConcurrentHashMap<>();
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

}
