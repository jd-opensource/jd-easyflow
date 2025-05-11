package com.jd.easyflow.flow.ext.check.impl;

/**
 * @author liyuliang5
 */
public class FlowNodeLinkCheckConfig {

    private boolean checkNodeIsolated;
    
    private boolean checkNextNodesNotExists;
    
    private boolean checkPreCheckNodesNotExists;

    private boolean checkNonStartNodeNoPrevious;
    
    private boolean checkNonEndNodeNoNext;


    public boolean isCheckNodeIsolated() {
        return checkNodeIsolated;
    }

    public void setCheckNodeIsolated(boolean checkNodeIsolated) {
        this.checkNodeIsolated = checkNodeIsolated;
    }

    public boolean isCheckNextNodesNotExists() {
        return checkNextNodesNotExists;
    }

    public void setCheckNextNodesNotExists(boolean checkNextNodesNotExists) {
        this.checkNextNodesNotExists = checkNextNodesNotExists;
    }

    public boolean isCheckPreCheckNodesNotExists() {
        return checkPreCheckNodesNotExists;
    }

    public void setCheckPreCheckNodesNotExists(boolean checkPreCheckNodesNotExists) {
        this.checkPreCheckNodesNotExists = checkPreCheckNodesNotExists;
    }

    public boolean isCheckNonStartNodeNoPrevious() {
        return checkNonStartNodeNoPrevious;
    }

    public void setCheckNonStartNodeNoPrevious(boolean checkNonStartNodeNoPrevious) {
        this.checkNonStartNodeNoPrevious = checkNonStartNodeNoPrevious;
    }

    public boolean isCheckNonEndNodeNoNext() {
        return checkNonEndNodeNoNext;
    }

    public void setCheckNonEndNodeNoNext(boolean checkNonEndNodeNoNext) {
        this.checkNonEndNodeNoNext = checkNonEndNodeNoNext;
    }
  
    
}
