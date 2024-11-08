package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * 
 * @author liyuliang5
 */
public class CreateProcessInstanceRes implements Serializable {

    private String processInstanceNo;

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    @Override
    public String toString() {
        return "CreateProcessInstanceRes [processInstanceNo=" + processInstanceNo + "]";
    }
    
    
}
