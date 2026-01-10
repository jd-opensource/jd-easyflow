package com.jd.easyflow.processunit.domain.model.entity;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 * 
 */
public class ProcessUnitEntity {

    private String processUnitCode;

    private String parentCode;

    private String name;

    private Map<String, Object> configMap;

    private String status;


    public Object getConfig(String key) {
        if (configMap == null) {
            return null;
        }
        return configMap.get(key);
    }


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


    public Map<String, Object> getConfigMap() {
        return configMap;
    }


    public void setConfigMap(Map<String, Object> configMap) {
        this.configMap = configMap;
    }


    public String getStatus() {
        return status;
    }


    public void setStatus(String status) {
        this.status = status;
    }


    @Override
    public String toString() {
        return "ProcessUnitEntity [processUnitCode=" + processUnitCode + ", parentCode=" + parentCode + ", name=" + name
                + ", configMap=" + configMap + ", status=" + status + "]";
    }
    
    

}
