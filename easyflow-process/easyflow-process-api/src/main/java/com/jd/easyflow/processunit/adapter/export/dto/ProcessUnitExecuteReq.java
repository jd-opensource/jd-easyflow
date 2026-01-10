package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUnitExecuteReq implements Serializable {

    String processUnitInstanceNo;
    
    String unitCode;
    
    String bizNo;
    
    private Map<String, String> clientInfo;

    public String getProcessUnitInstanceNo() {
        return processUnitInstanceNo;
    }

    public void setProcessUnitInstanceNo(String processUnitInstanceNo) {
        this.processUnitInstanceNo = processUnitInstanceNo;
    }
    
    public String getUnitCode() {
        return unitCode;
    }

    public void setUnitCode(String unitCode) {
        this.unitCode = unitCode;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }
    
    public Map<String, String> getClientInfo() {
        return clientInfo;
    }

    public void setClientInfo(Map<String, String> clientInfo) {
        this.clientInfo = clientInfo;
    }

    @Override
    public String toString() {
        return "ProcessUnitExecuteReq [processUnitInstanceNo=" + processUnitInstanceNo + ", unitCode=" + unitCode
                + ", bizNo=" + bizNo + "]";
    }

    
}
