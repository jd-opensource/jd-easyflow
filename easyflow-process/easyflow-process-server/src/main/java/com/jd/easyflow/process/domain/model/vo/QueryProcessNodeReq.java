package com.jd.easyflow.process.domain.model.vo;

import java.io.Serializable;
import java.util.Set;

/**
 * @author liyuliang5
 *
 */
public class QueryProcessNodeReq implements Serializable {

    private static final long serialVersionUID = -1229795408236692680L;
    
    public QueryProcessNodeReq() {
        
    }
    
    public QueryProcessNodeReq(String processInstanceNo, String nodeId, Set<String> status) {
        this.processInstanceNo = processInstanceNo;
        this.nodeId = nodeId;
        this.status = status;
    }

    private String processInstanceNo;

    private String nodeId;

    private Set<String> status;

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getNodeId() {
        return nodeId;
    }

    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }

    public Set<String> getStatus() {
        return status;
    }

    public void setStatus(Set<String> status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "QueryProcessNodeReq [processInstanceNo=" + processInstanceNo + ", nodeId=" + nodeId + ", status="
                + status + "]";
    }
    
    public static QueryProcessNodeReq.QueryProcessNodeReqBuilder builder() {
        return new QueryProcessNodeReq.QueryProcessNodeReqBuilder();
    }
    

    public static class QueryProcessNodeReqBuilder {
        private String processInstanceNo;
        private String nodeId;
        private Set<String> status;

        public QueryProcessNodeReq.QueryProcessNodeReqBuilder processInstanceNo(String processInstanceNo) {
            this.processInstanceNo = processInstanceNo;
            return this;
        }

        public QueryProcessNodeReq.QueryProcessNodeReqBuilder nodeId(String nodeId) {
            this.nodeId = nodeId;
            return this;
        }

        public QueryProcessNodeReq.QueryProcessNodeReqBuilder status(Set<String> status) {
            this.status = status;
            return this;
        }

        public QueryProcessNodeReq build() {
            return new QueryProcessNodeReq(this.processInstanceNo, this.nodeId, this.status);
        }

        public String toString() {
            return "QueryProcessNodeReq.QueryProcessNodeReqBuilder(processInstanceNo=" + this.processInstanceNo
                    + ", nodeId=" + this.nodeId + ", status=" + this.status + ")";
        }
    }

}
