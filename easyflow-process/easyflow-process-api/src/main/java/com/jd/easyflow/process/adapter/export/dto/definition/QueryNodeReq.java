package com.jd.easyflow.process.adapter.export.dto.definition;

import java.io.Serializable;

/**
 * 
 * @author liyuliang5
 *
 */
public class QueryNodeReq implements Serializable {
    

    private String processId;
    
    private String nodeId;
    
    public QueryNodeReq() {
        // NOOP
    }
    
    public QueryNodeReq(String processId, String nodeId) {
        this.processId = processId;
        this.nodeId = nodeId;
    }

    public String getProcessId() {
        return processId;
    }

    public void setProcessId(String processId) {
        this.processId = processId;
    }

    public String getNodeId() {
        return nodeId;
    }

    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }

    @Override
    public String toString() {
        return "QueryNodeReq [processId=" + processId + ", nodeId=" + nodeId + "]";
    }
    
    
}
