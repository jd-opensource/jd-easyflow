package com.jd.easyflow.process.domain.model.vo;

/**
 * 
 * @author liyuliang5
 */
public class CreateProcessInstanceResVO {

    private String processInstanceNo;

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    @Override
    public String toString() {
        return "CreateProcessInstanceResVO [processInstanceNo=" + processInstanceNo + "]";
    }
    
    
}
