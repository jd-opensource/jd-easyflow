package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessUnitUpdateReq implements Serializable {
    
    public static final String UPDATE_TYPE_VARIABLES = "VARIABLES";
    
    public static final String UPDATE_TYPE_REQUEST_CONTENT = "REQUEST_CONTENT";
    
    private String instanceNo;
    
    private String unitCode;
    
    private String bizNo;
    
    private String result;
    
    private String responseContent;
    
    private Map<String, String> variables;
    
    private String requestContent;
    
    private Date requestTime;
    
    private Date responseTime;
    
    private String requestNo;
    
    private String execType;
    
    private Map<String, String> requestContext;
    
    private String productCode;
    
    private Boolean autoRunFlag;
    
    private Date nextAutoRunTime;
    
    private String updateType;
    private Boolean lock;
    
    private Map<String, String> clientInfo;
    
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

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public String getResponseContent() {
        return responseContent;
    }

    public void setResponseContent(String responseContent) {
        this.responseContent = responseContent;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }

    public String getRequestContent() {
        return requestContent;
    }

    public void setRequestContent(String requestContent) {
        this.requestContent = requestContent;
    }

    public Date getRequestTime() {
        return requestTime;
    }

    public void setRequestTime(Date requestTime) {
        this.requestTime = requestTime;
    }

    public Date getResponseTime() {
        return responseTime;
    }

    public void setResponseTime(Date responseTime) {
        this.responseTime = responseTime;
    }

    public String getRequestNo() {
        return requestNo;
    }

    public void setRequestNo(String requestNo) {
        this.requestNo = requestNo;
    }

    public String getExecType() {
        return execType;
    }

    public void setExecType(String execType) {
        this.execType = execType;
    }

    public Map<String, String> getRequestContext() {
        return requestContext;
    }

    public void setRequestContext(Map<String, String> requestContext) {
        this.requestContext = requestContext;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
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
    
    public String getUpdateType() {
        return updateType;
    }

    public void setUpdateType(String updateType) {
        this.updateType = updateType;
    }
    

    public Boolean getLock() {
        return lock;
    }

    public void setLock(Boolean lock) {
        this.lock = lock;
    }
    
    public Map<String, String> getClientInfo() {
        return clientInfo;
    }

    public void setClientInfo(Map<String, String> clientInfo) {
        this.clientInfo = clientInfo;
    }

    @Override
    public String toString() {
        return "ProcessUnitUpdateReq [instanceNo=" + instanceNo + ", unitCode=" + unitCode + ", bizNo=" + bizNo
                + ", result=" + result + ", responseContent=" + responseContent + ", variables=" + variables
                + ", requestContent=" + requestContent + ", requestTime=" + requestTime + ", responseTime="
                + responseTime + ", requestNo=" + requestNo + ", execType=" + execType + ", requestContext="
                + requestContext + ", productCode=" + productCode + ", autoRunFlag=" + autoRunFlag
                + ", nextAutoRunTime=" + nextAutoRunTime + ", updateType=" + updateType +  ", clientInfo=" + clientInfo + "]";
    }
    
    

    
}
