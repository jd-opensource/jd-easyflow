package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;
import java.util.Set;

/**
 * @author liyuliang5
 *
 */
public class QueryProcessNodeReqDTO implements Serializable {

    private String processInstanceNo;

    private String nodeId;

    private Set<String> status;
    
    public QueryProcessNodeReqDTO() {
        
    }
    
    public QueryProcessNodeReqDTO(String processInstanceNo, String nodeId, Set<String> status) {
        this.processInstanceNo = processInstanceNo;
        this.nodeId = nodeId;
        this.status = status;
    }

    public static QueryProcessNodeReqDTO.QueryProcessNodeReqDTOBuilder builder() {
        return new QueryProcessNodeReqDTO.QueryProcessNodeReqDTOBuilder();
    }

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
        return "QueryProcessNodeReqDTO [processInstanceNo=" + processInstanceNo + ", nodeId=" + nodeId + ", status="
                + status + "]";
    }
    

    public static class QueryProcessNodeReqDTOBuilder {
        private String processInstanceNo;
        private String nodeId;
        private Set<String> status;

        public QueryProcessNodeReqDTO.QueryProcessNodeReqDTOBuilder processInstanceNo(String processInstanceNo) {
            this.processInstanceNo = processInstanceNo;
            return this;
        }

        public QueryProcessNodeReqDTO.QueryProcessNodeReqDTOBuilder nodeId(String nodeId) {
            this.nodeId = nodeId;
            return this;
        }

        public QueryProcessNodeReqDTO.QueryProcessNodeReqDTOBuilder status(Set<String> status) {
            this.status = status;
            return this;
        }

        public QueryProcessNodeReqDTO build() {
            return new QueryProcessNodeReqDTO(this.processInstanceNo, this.nodeId, this.status);
        }

        public String toString() {
            return "QueryProcessNodeReqDTO.QueryProcessNodeReqDTOBuilder(processInstanceNo=" + this.processInstanceNo
                    + ", nodeId=" + this.nodeId + ", status=" + this.status + ")";
        }
    }
    
    
}
