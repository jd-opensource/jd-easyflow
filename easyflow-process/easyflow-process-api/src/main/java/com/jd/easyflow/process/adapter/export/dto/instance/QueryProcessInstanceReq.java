package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * 
 * @author liyuliang5
 *
 */
public class QueryProcessInstanceReq implements Serializable {
    
    public QueryProcessInstanceReq() {
        
    }
    
    public QueryProcessInstanceReq(String processType, String bizNo) {
        this.processType = processType;
        this.bizNo = bizNo;
        
    }

    private String processType;
    
    private String bizNo;

    public String getProcessType() {
        return processType;
    }

    public void setProcessType(String processType) {
        this.processType = processType;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    @Override
    public String toString() {
        return "QueryProcessInstanceReq [processType=" + processType + ", bizNo=" + bizNo + "]";
    }
    
    public static QueryProcessInstanceReq.QueryProcessInstanceReqBuilder builder() {
        return new QueryProcessInstanceReq.QueryProcessInstanceReqBuilder();
    }
    

    public static class QueryProcessInstanceReqBuilder {
        private String processType;
        private String bizNo;

        public QueryProcessInstanceReq.QueryProcessInstanceReqBuilder processType(String processType) {
            this.processType = processType;
            return this;
        }

        public QueryProcessInstanceReq.QueryProcessInstanceReqBuilder bizNo(String bizNo) {
            this.bizNo = bizNo;
            return this;
        }

        public QueryProcessInstanceReq build() {
            return new QueryProcessInstanceReq(this.processType, this.bizNo);
        }

        public String toString() {
            return "QueryProcessInstanceReq.QueryProcessInstanceReqBuilder(processType=" + this.processType + ", bizNo="
                    + this.bizNo + ")";
        }
    }
    
    
}
