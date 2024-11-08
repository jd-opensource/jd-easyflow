package com.jd.easyflow.process.adapter.export.dto.definition;

import java.io.Serializable;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class NodeDTO implements Serializable {

    private Map<String, Object> properties;

    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        return "NodeDTO [properties=" + properties + "]";
    }
    
    
}
