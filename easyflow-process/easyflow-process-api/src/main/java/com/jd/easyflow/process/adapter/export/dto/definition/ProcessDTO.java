package com.jd.easyflow.process.adapter.export.dto.definition;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessDTO implements Serializable {
    
    private List<NodeDTO> nodeList;

    private Map<String, Object> properties;

    public List<NodeDTO> getNodeList() {
        return nodeList;
    }

    public void setNodeList(List<NodeDTO> nodeList) {
        this.nodeList = nodeList;
    }

    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        return "ProcessDTO [nodeList=" + nodeList + ", properties=" + properties + "]";
    }
    
    
}
