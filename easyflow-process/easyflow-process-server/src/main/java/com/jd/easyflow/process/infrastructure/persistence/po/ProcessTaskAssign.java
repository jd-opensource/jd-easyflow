package com.jd.easyflow.process.infrastructure.persistence.po;

import java.util.Date;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTaskAssign {
    
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

    private boolean deleted;

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

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }
    
    public String getAssignGroup2() {
        return assignGroup2;
    }

    public void setAssignGroup2(String assignGroup2) {
        this.assignGroup2 = assignGroup2;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName());
        sb.append(" [");
        sb.append("Hash = ").append(hashCode());
        sb.append(", id=").append(id);
        sb.append(", assignNo=").append(assignNo);
        sb.append(", taskNo=").append(taskNo);
        sb.append(", assignType=").append(assignType);
        sb.append(", assignGroup=").append(assignGroup);
        sb.append(", assignGroup2=").append(assignGroup2);
        sb.append(", assignUser=").append(assignUser);
        sb.append(", status=").append(status);
        sb.append(", assignTime=").append(assignTime);
        sb.append(", productCode=").append(productCode);
        sb.append(", extData=").append(extData);
        sb.append(", createdDate=").append(createdDate);
        sb.append(", modifiedDate=").append(modifiedDate);
        sb.append(", deleted=").append(deleted);
        sb.append("]");
        return sb.toString();
    }
}