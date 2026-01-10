package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;

/**
 * 
 * @author liyuliang5
 */
public class ShardingInfoQueryReq implements Serializable {

    private String unitCode;
    
    private String bizNo;

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
        return "ShardingInfoQueryReq [unitCode=" + unitCode + ", bizNo=" + bizNo + "]";
    }
    
    
}
