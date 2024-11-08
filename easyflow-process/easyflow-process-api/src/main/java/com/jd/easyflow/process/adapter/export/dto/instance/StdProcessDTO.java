package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class StdProcessDTO implements Serializable {

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

    @Override
    public String toString() {
        return "StdProcessDTO [processProperties=" + processProperties + ", extProperties=" + extProperties + "]";
    }
    
    
}
