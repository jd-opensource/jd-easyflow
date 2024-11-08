package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class QueryOpenNodeInstanceReq implements Serializable {

    private String instanceNo;

    private String nodeId;
    
    public QueryOpenNodeInstanceReq() {
        
    }
    
    public QueryOpenNodeInstanceReq(String instanceNo, String nodeId) {
        this.instanceNo = instanceNo;
        this.nodeId = nodeId;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    public String getNodeId() {
        return nodeId;
    }

    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }

    @Override
    public String toString() {
        return "QueryOpenNodeInstanceReq [instanceNo=" + instanceNo + ", nodeId=" + nodeId + "]";
    }
    

    public static QueryOpenNodeInstanceReq.QueryOpenNodeInstanceReqBuilder builder() {
        return new QueryOpenNodeInstanceReq.QueryOpenNodeInstanceReqBuilder();
    }
    

    public static class QueryOpenNodeInstanceReqBuilder {
        private String instanceNo;
        private String nodeId;

        public QueryOpenNodeInstanceReq.QueryOpenNodeInstanceReqBuilder instanceNo(String instanceNo) {
            this.instanceNo = instanceNo;
            return this;
        }

        public QueryOpenNodeInstanceReq.QueryOpenNodeInstanceReqBuilder nodeId(String nodeId) {
            this.nodeId = nodeId;
            return this;
        }

        public QueryOpenNodeInstanceReq build() {
            return new QueryOpenNodeInstanceReq(this.instanceNo, this.nodeId);
        }

        public String toString() {
            return "QueryOpenNodeInstanceReq.QueryOpenNodeInstanceReqBuilder(instanceNo=" + this.instanceNo
                    + ", nodeId=" + this.nodeId + ")";
        }
    }
    
    
}
