package com.jd.easyflow.processunit.domain.model.vo;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUnitInstanceKey {

    private String instanceNo;
    
    private String unitCode;
    
    private String bizNo;
    
    public ProcessUnitInstanceKey() {
        
    }
    
    public ProcessUnitInstanceKey(String instanceNo) {
        this.instanceNo = instanceNo;
    }
    
    public ProcessUnitInstanceKey(String unitCode, String bizNo) {
        this.unitCode = unitCode;
        this.bizNo = bizNo;
    }
    
    public ProcessUnitInstanceKey(String instanceNo, String unitCode, String bizNo) {
        this.instanceNo = instanceNo;
        this.unitCode = unitCode;
        this.bizNo = bizNo;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
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

    @Override
    public String toString() {
        return "ProcessUnitInstanceKey [instanceNo=" + instanceNo + ", unitCode=" + unitCode + ", bizNo=" + bizNo + "]";
    }
    
    
}
