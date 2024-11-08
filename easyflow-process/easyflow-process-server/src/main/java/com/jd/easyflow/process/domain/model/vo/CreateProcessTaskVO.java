package com.jd.easyflow.process.domain.model.vo;

import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class CreateProcessTaskVO {

    private String processType;
    
    private String bizNo;
    
    private String taskType;
    
    private String taskBizCode;
    
    private String taskBizName;
    
    private String processInstanceNo;
    
    private String nodeInstanceNo;
    
    private String assignType;
    
    private Map<String, Object> assignInfo;
    
    private String creator;
    
    private String productCode;
        
    private Object extData;

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

    public String getTaskType() {
        return taskType;
    }

    public void setTaskType(String taskType) {
        this.taskType = taskType;
    }

    public String getTaskBizCode() {
        return taskBizCode;
    }

    public void setTaskBizCode(String taskBizCode) {
        this.taskBizCode = taskBizCode;
    }

    public String getTaskBizName() {
        return taskBizName;
    }

    public void setTaskBizName(String taskBizName) {
        this.taskBizName = taskBizName;
    }

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getNodeInstanceNo() {
        return nodeInstanceNo;
    }

    public void setNodeInstanceNo(String nodeInstanceNo) {
        this.nodeInstanceNo = nodeInstanceNo;
    }

    public String getAssignType() {
        return assignType;
    }

    public void setAssignType(String assignType) {
        this.assignType = assignType;
    }

    public Map<String, Object> getAssignInfo() {
        return assignInfo;
    }

    public void setAssignInfo(Map<String, Object> assignInfo) {
        this.assignInfo = assignInfo;
    }

    public String getCreator() {
        return creator;
    }

    public void setCreator(String creator) {
        this.creator = creator;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public Object getExtData() {
        return extData;
    }

    public void setExtData(Object extData) {
        this.extData = extData;
    }

    @Override
    public String toString() {
        return "CreateProcessTaskVO [processType=" + processType + ", bizNo=" + bizNo + ", taskType=" + taskType
                + ", taskBizCode=" + taskBizCode + ", taskBizName=" + taskBizName + ", processInstanceNo="
                + processInstanceNo + ", nodeInstanceNo=" + nodeInstanceNo + ", assignType=" + assignType
                + ", assignInfo=" + assignInfo + ", creator=" + creator + ", productCode=" + productCode + ", extData="
                + extData + "]";
    }
    
    
}
