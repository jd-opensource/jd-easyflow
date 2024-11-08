package com.jd.easyflow.process.spi.client.dto;

import java.io.Serializable;

/**
 * 
 * @author liyuliang5
 */
public class ProcessExecuteRes implements Serializable {

    private String processInstanceNo;

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    @Override
    public String toString() {
        return "ProcessExecuteRes [processInstanceNo=" + processInstanceNo + "]";
    }
    
    
}
