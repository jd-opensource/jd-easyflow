package com.jd.easyflow.process.adapter.export.dto.schedule;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 */
public class ScheduleProcessRes implements Serializable {
    
    private String processInstanceNo;
    
    private Object result;
    
    private Map<String, Object> dataMap;

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public Object getResult() {
        return result;
    }

    public void setResult(Object result) {
        this.result = result;
    }

    public Map<String, Object> getDataMap() {
        return dataMap;
    }

    public void setDataMap(Map<String, Object> dataMap) {
        this.dataMap = dataMap;
    }
    
}
