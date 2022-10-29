package com.jd.easyflow.fsm.model;

/**
 * Post handle result.
 * @author liyuliang5
 *
 */
public class PostHandleResult {

    private String postStateId;
    
    private String postEventId;
    
    public PostHandleResult() {
        // NOOP
    }
    
    public PostHandleResult(String postStateId) {
        this.postStateId = postStateId;
    }
    
    public PostHandleResult(String postStateId, String postEventId) {
        this.postStateId = postStateId;
        this.postEventId = postEventId;
    }

    public String getPostStateId() {
        return postStateId;
    }

    public void setPostStateId(String postStateId) {
        this.postStateId = postStateId;
    }

    public String getPostEventId() {
        return postEventId;
    }

    public void setPostEventId(String postEventId) {
        this.postEventId = postEventId;
    }
    
    
}
