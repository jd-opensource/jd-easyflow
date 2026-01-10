package com.jd.easyflow.processunit.domain.model.vo;

import java.util.Date;
import java.util.Map;

/**
 * @author liyuliang5
 * 
 */
public class ExecParam {

    private String unitCode;
    
    private String bizNo;
    
    private String requestNo;
                
    private String requestContent;
    
    private String instanceNo;
    
    private String executionNo;

    private String execType;

    private Map<String, Object> requestContext;
    
    private String productCode;

    private String version;

    private Boolean autoRunFlag;

    private Date nextAutoRunTime;
    
    private Map<String, String> variables;
    
    private Map<String, String> clientInfo;

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

    public String getRequestNo() {
        return requestNo;
    }

    public void setRequestNo(String requestNo) {
        this.requestNo = requestNo;
    }

    public String getRequestContent() {
        return requestContent;
    }

    public void setRequestContent(String requestContent) {
        this.requestContent = requestContent;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public String getExecType() {
        return execType;
    }

    public void setExecType(String execType) {
        this.execType = execType;
    }

    public Map<String, Object> getRequestContext() {
        return requestContext;
    }

    public void setRequestContext(Map<String, Object> requestContext) {
        this.requestContext = requestContext;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }
    
    

    public Boolean getAutoRunFlag() {
        return autoRunFlag;
    }

    public void setAutoRunFlag(Boolean autoRunFlag) {
        this.autoRunFlag = autoRunFlag;
    }

    public Date getNextAutoRunTime() {
        return nextAutoRunTime;
    }

    public void setNextAutoRunTime(Date nextAutoRunTime) {
        this.nextAutoRunTime = nextAutoRunTime;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }
    
    public Map<String, String> getClientInfo() {
        return clientInfo;
    }

    public void setClientInfo(Map<String, String> clientInfo) {
        this.clientInfo = clientInfo;
    }

    @Override
    public String toString() {
        return "ExecParam [unitCode=" + unitCode + ", bizNo=" + bizNo + ", requestNo=" + requestNo + ", requestContent="
                + requestContent + ", instanceNo=" + instanceNo + ", executionNo=" + executionNo + ", execType="
                + execType + ", requestContext=" + requestContext + ", productCode=" + productCode + ", version="
                + version + ", autoRunFlag=" + autoRunFlag + ", nextAutoRunTime=" + nextAutoRunTime + ", variables="
                + variables + ", clientInfo=" + clientInfo + "]";
    }

}
