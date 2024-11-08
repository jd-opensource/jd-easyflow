package com.jd.easyflow.admin.process.adapter.page.dto;

import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessDefDTO {

    private Long id;
    
    private String defId;

    private Integer defVersion;
    
    private String defName;
    
    private String format;
    
    private String bpmnXmlData;
    
    private String jsonData;

    private String bizType;

    private String category;

    private Boolean latest;

    private String defSource;
    
    private Date createdDate;

    private Date modifiedDate;

    private String createdBy;

    private String modifiedBy;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getDefId() {
        return defId;
    }

    public void setDefId(String defId) {
        this.defId = defId;
    }

    public Integer getDefVersion() {
        return defVersion;
    }

    public void setDefVersion(Integer defVersion) {
        this.defVersion = defVersion;
    }

    public String getDefName() {
        return defName;
    }

    public void setDefName(String defName) {
        this.defName = defName;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public String getBpmnXmlData() {
        return bpmnXmlData;
    }

    public void setBpmnXmlData(String bpmnXmlData) {
        this.bpmnXmlData = bpmnXmlData;
    }

    public String getJsonData() {
        return jsonData;
    }

    public void setJsonData(String jsonData) {
        this.jsonData = jsonData;
    }

    public String getBizType() {
        return bizType;
    }

    public void setBizType(String bizType) {
        this.bizType = bizType;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public Boolean getLatest() {
        return latest;
    }

    public void setLatest(Boolean latest) {
        this.latest = latest;
    }

    public String getDefSource() {
        return defSource;
    }

    public void setDefSource(String defSource) {
        this.defSource = defSource;
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

    public String getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(String createdBy) {
        this.createdBy = createdBy;
    }

    public String getModifiedBy() {
        return modifiedBy;
    }

    public void setModifiedBy(String modifiedBy) {
        this.modifiedBy = modifiedBy;
    }

    @Override
    public String toString() {
        return "ProcessDefDTO [id=" + id + ", defId=" + defId + ", defVersion=" + defVersion + ", defName=" + defName
                + ", format=" + format + ", bpmnXmlData=" + bpmnXmlData + ", jsonData=" + jsonData + ", bizType="
                + bizType + ", category=" + category + ", latest=" + latest + ", defSource=" + defSource
                + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + ", createdBy=" + createdBy
                + ", modifiedBy=" + modifiedBy + "]";
    }
    
    
   
}
