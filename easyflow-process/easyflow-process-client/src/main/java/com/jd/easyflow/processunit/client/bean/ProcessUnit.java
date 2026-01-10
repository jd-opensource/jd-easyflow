package com.jd.easyflow.processunit.client.bean;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnit {
    
    private String processUnitCode;
    
    private String parentNo;
    
    private String name;
    
    private String config;
    
    private String status;

    public String getProcessUnitCode() {
        return processUnitCode;
    }

    public void setProcessUnitCode(String processUnitCode) {
        this.processUnitCode = processUnitCode;
    }

    public String getParentNo() {
        return parentNo;
    }

    public void setParentNo(String parentNo) {
        this.parentNo = parentNo;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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

    @Override
    public String toString() {
        return "ProcessUnit [processUnitCode=" + processUnitCode + ", parentNo=" + parentNo + ", name=" + name
                + ", config=" + config + ", status=" + status + "]";
    }
    
    
}
