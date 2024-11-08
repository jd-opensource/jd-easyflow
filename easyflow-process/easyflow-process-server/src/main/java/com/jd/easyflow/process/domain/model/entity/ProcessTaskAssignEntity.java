package com.jd.easyflow.process.domain.model.entity;

import java.util.Date;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTaskAssignEntity {
    
    private Long id;

    private String assignNo;

    private String taskNo;

    private String assignType;

    private String assignGroup;
    
    private String assignGroup2;

    private String assignUser;

    private String status;

    private Date assignTime;

    private String productCode;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getAssignNo() {
        return assignNo;
    }

    public void setAssignNo(String assignNo) {
        this.assignNo = assignNo;
    }

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }

    public String getAssignType() {
        return assignType;
    }

    public void setAssignType(String assignType) {
        this.assignType = assignType;
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

    public String getAssignUser() {
        return assignUser;
    }

    public void setAssignUser(String assignUser) {
        this.assignUser = assignUser;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Date getAssignTime() {
        return assignTime;
    }

    public void setAssignTime(Date assignTime) {
        this.assignTime = assignTime;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getExtData() {
        return extData;
    }

    public void setExtData(String extData) {
        this.extData = extData;
    }

    public Date getCreatedDate() {
        return createdDate;
    }

    public void setCreatedDate(Date createdDate) {
        this.createdDate = createdDate;
    }

    public Date getModifiedDate() {
        return modifiedDate;
    }

    public void setModifiedDate(Date modifiedDate) {
        this.modifiedDate = modifiedDate;
    }

    @Override
    public String toString() {
        return "ProcessTaskAssignEntity [id=" + id + ", assignNo=" + assignNo + ", taskNo=" + taskNo + ", assignType="
                + assignType + ", assignGroup=" + assignGroup + ", assignGroup2=" + assignGroup2 + ", assignUser="
                + assignUser + ", status=" + status + ", assignTime=" + assignTime + ", productCode=" + productCode
                + ", extData=" + extData + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + "]";
    }
    
    

}
