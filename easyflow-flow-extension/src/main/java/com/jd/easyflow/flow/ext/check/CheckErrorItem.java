package com.jd.easyflow.flow.ext.check;

import com.jd.easyflow.flow.model.Flow;

/**
 * @author liyuliang5
 */
public class CheckErrorItem {

    String errorType;
    String errorMessage;
    String flowId;
    String nodeId;
    Flow flow;
    
    public CheckErrorItem() {
        
    }
    
    public CheckErrorItem(String errorType, String errorMessage, String flowId, String nodeId, Flow flow) {
        this.errorType = errorType;
        this.errorMessage = errorMessage;
        this.flowId = flowId;
        this.nodeId = nodeId;
        this.flow = flow;
    }
    
    public String getErrorType() {
        return errorType;
    }
    public void setErrorType(String errorType) {
        this.errorType = errorType;
    }
    public String getErrorMessage() {
        return errorMessage;
    }
    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }
    public String getFlowId() {
        return flowId;
    }
    public void setFlowId(String flowId) {
        this.flowId = flowId;
    }
    public String getNodeId() {
        return nodeId;
    }
    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }

    public Flow getFlow() {
        return flow;
    }

    public void setFlow(Flow flow) {
        this.flow = flow;
    }
    
    
    
    
}
