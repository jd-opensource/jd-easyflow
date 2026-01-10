package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUnitDTO implements Serializable {

    private String processUnitCode;

    private String parentCode;

    private String name;

    private String config;

    private String status;

    public String getProcessUnitCode() {
        return processUnitCode;
    }

    public void setProcessUnitCode(String processUnitCode) {
        this.processUnitCode = processUnitCode;
    }

    public String getParentCode() {
        return parentCode;
    }

    public void setParentCode(String parentCode) {
        this.parentCode = parentCode;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getConfig() {
        return config;
    }

    public void setConfig(String config) {
        this.config = config;
    }
    
}
