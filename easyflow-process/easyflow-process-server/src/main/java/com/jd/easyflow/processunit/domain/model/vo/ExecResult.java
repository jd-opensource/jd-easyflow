package com.jd.easyflow.processunit.domain.model.vo;

import java.util.Date;
import java.util.Map;

/**
 * @author liyuliang5
 * 
 */
public class ExecResult {
    
    public ExecResult() {
        
    }
    
    public ExecResult(String result, String responseContent, Map<String, String> variables) {
        this.result = result;
        this.responseContent = responseContent;
        this.variables = variables;
    }
    
    private String result;
    
    private String responseContent;
    
    private Map<String, String> variables;
    
    private Boolean autoRunFlag;
    
    private Date nextAutoRunTime;

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public String getResponseContent() {
        return responseContent;
    }

    public void setResponseContent(String responseContent) {
        this.responseContent = responseContent;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }
    

    public Boolean getAutoRunFlag() {
        return autoRunFlag;
    }

    public void setAutoRunFlag(Boolean autoRunFlag) {
        this.autoRunFlag = autoRunFlag;
    }

    public Date getNextAutoRunTime() {
        return nextAutoRunTime;
    }

    public void setNextAutoRunTime(Date nextAutoRunTime) {
        this.nextAutoRunTime = nextAutoRunTime;
    }

    @Override
    public String toString() {
        return "ExecResult [result=" + result + ", responseContent=" + responseContent + ", variables=" + variables
                + ", autoRunFlag=" + autoRunFlag + ", nextAutoRunTime=" + nextAutoRunTime + "]";
    }

    
}
