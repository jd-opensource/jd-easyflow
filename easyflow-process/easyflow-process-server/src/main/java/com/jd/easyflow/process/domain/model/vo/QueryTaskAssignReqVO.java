package com.jd.easyflow.process.domain.model.vo;

import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class QueryTaskAssignReqVO {

    private Long id;

    private String assignNo;

    private String taskNo;

    private String assignType;

    private String assignGroup;
    
    private String assignGroup2;

    private String assignUser;

    private String status;

    private String assignTimeStart;
    private String assignTimeEnd;
    
    private String productCode;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;
    
    private String createdDateStart;
    
    private String createdDateEnd;
    
    private String modifiedDateStart;
    
    private String modifiedDateEnd;

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

    public String getAssignTimeStart() {
        return assignTimeStart;
    }

    public void setAssignTimeStart(String assignTimeStart) {
        this.assignTimeStart = assignTimeStart;
    }

    public String getAssignTimeEnd() {
        return assignTimeEnd;
    }

    public void setAssignTimeEnd(String assignTimeEnd) {
        this.assignTimeEnd = assignTimeEnd;
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

    public String getCreatedDateStart() {
        return createdDateStart;
    }

    public void setCreatedDateStart(String createdDateStart) {
        this.createdDateStart = createdDateStart;
    }

    public String getCreatedDateEnd() {
        return createdDateEnd;
    }

    public void setCreatedDateEnd(String createdDateEnd) {
        this.createdDateEnd = createdDateEnd;
    }

    public String getModifiedDateStart() {
        return modifiedDateStart;
    }

    public void setModifiedDateStart(String modifiedDateStart) {
        this.modifiedDateStart = modifiedDateStart;
    }

    public String getModifiedDateEnd() {
        return modifiedDateEnd;
    }

    public void setModifiedDateEnd(String modifiedDateEnd) {
        this.modifiedDateEnd = modifiedDateEnd;
    }

    @Override
    public String toString() {
        return "QueryTaskAssignReqVO [id=" + id + ", assignNo=" + assignNo + ", taskNo=" + taskNo + ", assignType="
                + assignType + ", assignGroup=" + assignGroup + ", assignGroup2=" + assignGroup2 + ", assignUser="
                + assignUser + ", status=" + status + ", assignTimeStart=" + assignTimeStart + ", assignTimeEnd="
                + assignTimeEnd + ", productCode=" + productCode + ", extData=" + extData + ", createdDate="
                + createdDate + ", modifiedDate=" + modifiedDate + ", createdDateStart=" + createdDateStart
                + ", createdDateEnd=" + createdDateEnd + ", modifiedDateStart=" + modifiedDateStart
                + ", modifiedDateEnd=" + modifiedDateEnd + "]";
    }
    
    
}
