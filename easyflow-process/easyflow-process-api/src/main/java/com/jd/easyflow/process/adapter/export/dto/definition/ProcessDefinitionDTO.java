package com.jd.easyflow.process.adapter.export.dto.definition;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessDefinitionDTO implements Serializable {

    private Long id;

    private String defId;

    private Integer defVersion;

    private String name;

    private String format;

    private String bizType;

    private String jsonContent;

    private Boolean latest;

    private String defSource;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    private String content;

    private String category;

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

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public String getBizType() {
        return bizType;
    }

    public void setBizType(String bizType) {
        this.bizType = bizType;
    }

    public String getJsonContent() {
        return jsonContent;
    }

    public void setJsonContent(String jsonContent) {
        this.jsonContent = jsonContent;
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

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
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
        return "ProcessDefinitionDTO [id=" + id + ", defId=" + defId + ", defVersion=" + defVersion + ", name=" + name
                + ", format=" + format + ", bizType=" + bizType + ", jsonContent=" + jsonContent + ", latest=" + latest
                + ", defSource=" + defSource + ", extData=" + extData + ", createdDate=" + createdDate
                + ", modifiedDate=" + modifiedDate + ", content=" + content + ", category=" + category + ", createdBy="
                + createdBy + ", modifiedBy=" + modifiedBy + "]";
    }
    
    
}
