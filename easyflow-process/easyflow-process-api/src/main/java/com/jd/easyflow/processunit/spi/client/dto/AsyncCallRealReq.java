 package com.jd.easyflow.processunit.spi.client.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
 public class AsyncCallRealReq implements Serializable {

     private String executeExp;
     
     private String requestContent;
     
     private String executionNo;
     
     private Map<String, Object> requestContext;
     
     private String instanceNo;
     
     private String unitCode;
     
     private String bizNo;
     
     private String lastResult;
     
     private Map<String, String> variables;
     
     private Map<String, Object> serviceConf;

    public String getExecuteExp() {
        return executeExp;
    }

    public void setExecuteExp(String executeExp) {
        this.executeExp = executeExp;
    }

    public String getRequestContent() {
        return requestContent;
    }

    public void setRequestContent(String requestContent) {
        this.requestContent = requestContent;
    }

    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public Map<String, Object> getRequestContext() {
        return requestContext;
    }

    public void setRequestContext(Map<String, Object> requestContext) {
        this.requestContext = requestContext;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    public String getUnitCode() {
        return unitCode;
    }

    public void setUnitCode(String unitCode) {
        this.unitCode = unitCode;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getLastResult() {
        return lastResult;
    }

    public void setLastResult(String lastResult) {
        this.lastResult = lastResult;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }

    public Map<String, Object> getServiceConf() {
        return serviceConf;
    }

    public void setServiceConf(Map<String, Object> serviceConf) {
        this.serviceConf = serviceConf;
    }

    @Override
    public String toString() {
        return "AsyncCallRealReq [executeExp=" + executeExp + ", requestContent=" + requestContent + ", executionNo="
                + executionNo + ", requestContext=" + requestContext + ", instanceNo=" + instanceNo + ", unitCode="
                + unitCode + ", bizNo=" + bizNo + ", lastResult=" + lastResult + ", variables=" + variables
                + ", serviceConf=" + serviceConf + "]";
    }
     
     
     
}
