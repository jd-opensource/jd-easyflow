package com.jd.easyflow.process.client.runtime;

import java.util.Map;

import com.jd.easyflow.process.client.runtime.core.Node;

/**
 * @author liyuliang5
 * 
 */
public class StdNode implements Node {
    
    private Map<String, Object> processProperties;

    public Map<String, Object> getProcessProperties() {
        return processProperties;
    }

    public void setProcessProperties(Map<String, Object> processProperties) {
        this.processProperties = processProperties;
    }

    @Override
    public String toString() {
        return "StdNode [processProperties=" + processProperties + "]";
    }
    
    
}
