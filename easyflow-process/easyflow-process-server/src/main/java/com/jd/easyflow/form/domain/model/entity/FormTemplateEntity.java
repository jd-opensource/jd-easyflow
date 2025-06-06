package com.jd.easyflow.form.domain.model.entity;

import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class FormTemplateEntity {

    private Long id;

    private Date createdDate;

    private Date modifiedDate;

    private String templateCode;

    private String templateName;

    private String config;

    private String status;
    
    private String bizType;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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

    public String getTemplateCode() {
        return templateCode;
    }

    public void setTemplateCode(String templateCode) {
        this.templateCode = templateCode;
    }

    public String getTemplateName() {
        return templateName;
    }

    public void setTemplateName(String templateName) {
        this.templateName = templateName;
    }

    public String getConfig() {
        return config;
    }

    public void setConfig(String config) {
        this.config = config;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getBizType() {
        return bizType;
    }

    public void setBizType(String bizType) {
        this.bizType = bizType;
    }

    @Override
    public String toString() {
        return "FormTemplateEntity [id=" + id + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate
                + ", templateCode=" + templateCode + ", templateName=" + templateName + ", config=" + config
                + ", status=" + status + ", bizType=" + bizType + "]";
    }
    
    
}
