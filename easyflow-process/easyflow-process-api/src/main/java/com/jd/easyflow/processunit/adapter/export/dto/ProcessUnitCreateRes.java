package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitCreateRes implements Serializable {
    
    String processUnitInstanceNo;

    public String getProcessUnitInstanceNo() {
        return processUnitInstanceNo;
    }

    public void setProcessUnitInstanceNo(String processUnitInstanceNo) {
        this.processUnitInstanceNo = processUnitInstanceNo;
    }

    @Override
    public String toString() {
        return "ProcessUnitCreateRes [processUnitInstanceNo=" + processUnitInstanceNo + "]";
    }
    
    
}
