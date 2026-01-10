package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitInstanceQueryReq implements Serializable {

    private String bizNo;
    
    private String processUnitCode;

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getProcessUnitCode() {
        return processUnitCode;
    }

    public void setProcessUnitCode(String processUnitCode) {
        this.processUnitCode = processUnitCode;
    }

    @Override
    public String toString() {
        return "ProcessUnitInstanceQueryReq [bizNo=" + bizNo + ", processUnitCode=" + processUnitCode + "]";
    }
    
    
}
