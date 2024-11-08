package com.jd.easyflow.process.client.runtime;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.process.client.runtime.core.Process;

/**
 * @author liyuliang5
 * 
 */
public class StdProcess implements Process {
        
    private Map<String, Object> processProperties;
    
    private Map<String, Object> extProperties;

    public Map<String, Object> getProcessProperties() {
        return processProperties;
    }

    public void setProcessProperties(Map<String, Object> processProperties) {
        this.processProperties = processProperties;
    }

    public Map<String, Object> getExtProperties() {
        return extProperties;
    }

    public void setExtProperties(Map<String, Object> extProperties) {
        this.extProperties = extProperties;
    }
    
    public void putExtProperty(String key, Object value) {
        if (extProperties == null) {
            extProperties = new ConcurrentHashMap<>();
        }
        extProperties.put(key, value);
    }
    
    public <T>T getExtProperty(String key) {
        if (extProperties == null) {
            return null;
        }
        return (T) extProperties.get(key);
    }

    @Override
    public String toString() {
        return "StdProcess [processProperties=" + processProperties + ", extProperties=" + extProperties + "]";
    }
    
    
    
}
