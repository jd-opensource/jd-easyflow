package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitCreateReq implements Serializable {
	private static final long serialVersionUID = 1L;

	private String unitCode;
    
    private String bizNo;
    
    private String requestNo;
    
    private String requestContent;
    
    private String productCode;
    
    private String parentNo;
    
    private Boolean autoRunFlag;
    
    private Date nextAutoRunTime;
    
    private Map<String, String> variables;
    
    private Map<String, String> clientInfo;
    
    public ProcessUnitCreateReq() {
        
    }
    
    public ProcessUnitCreateReq(String unitCode, String bizNo, String requestContent) {
        this.unitCode = unitCode;
        this.bizNo = bizNo;
        this.requestContent = requestContent;
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

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getParentNo() {
        return parentNo;
    }

    public void setParentNo(String parentNo) {
        this.parentNo = parentNo;
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
        return "ProcessUnitCreateReq [unitCode=" + unitCode + ", bizNo=" + bizNo + ", requestNo=" + requestNo
                + ", requestContent=" + requestContent + ", productCode=" + productCode + ", parentNo=" + parentNo
                + ", autoRunFlag=" + autoRunFlag + ", nextAutoRunTime=" + nextAutoRunTime + ", variables=" + variables
                + ", clientInfo=" + clientInfo + "]";
    }

}
