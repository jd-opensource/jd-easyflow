package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskAssignDTO implements Serializable {


    private String assignType;
    
    private String assignUser;

    private String assignGroup;
    
    private String assignGroup2;
    
    private String assignNo;
    
    private Date assignTime;
    
    private Date createdDate;
    
    private String extData;
    
    private String productCode;
    
    private Date modifiedDate;
    
    private String status;
    
    private String taskNo;

    public String getAssignType() {
        return assignType;
    }

    public void setAssignType(String assignType) {
        this.assignType = assignType;
    }

    public String getAssignUser() {
        return assignUser;
    }

    public void setAssignUser(String assignUser) {
        this.assignUser = assignUser;
    }

    public String getAssignGroup() {
        return assignGroup;
    }

    public void setAssignGroup(String assignGroup) {
        this.assignGroup = assignGroup;
    }

    public String getAssignGroup2() {
        return assignGroup2;
    }

    public void setAssignGroup2(String assignGroup2) {
        this.assignGroup2 = assignGroup2;
    }

    public String getAssignNo() {
        return assignNo;
    }

    public void setAssignNo(String assignNo) {
        this.assignNo = assignNo;
    }

    public Date getAssignTime() {
        return assignTime;
    }

    public void setAssignTime(Date assignTime) {
        this.assignTime = assignTime;
    }

    public Date getCreatedDate() {
        return createdDate;
    }

    public void setCreatedDate(Date createdDate) {
        this.createdDate = createdDate;
    }

    public String getExtData() {
        return extData;
    }

    public void setExtData(String extData) {
        this.extData = extData;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public Date getModifiedDate() {
        return modifiedDate;
    }

    public void setModifiedDate(Date modifiedDate) {
        this.modifiedDate = modifiedDate;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }

    @Override
    public String toString() {
        return "ProcessTaskAssignDTO [assignType=" + assignType + ", assignUser=" + assignUser + ", assignGroup="
                + assignGroup + ", assignGroup2=" + assignGroup2 + ", assignNo=" + assignNo + ", assignTime="
                + assignTime + ", createdDate=" + createdDate + ", extData=" + extData + ", productCode=" + productCode
                + ", modifiedDate=" + modifiedDate + ", status=" + status + ", taskNo=" + taskNo + "]";
    }
    
    
    
}
