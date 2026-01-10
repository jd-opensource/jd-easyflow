 package com.jd.easyflow.processunit.spi.client.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
 public class AsyncCallRealRes implements Serializable {

     private String result;
     
     private String responseContent;
     
     private Map<String, String> variables;
     
     private Boolean autoRunFlag;
     
     private Date nextAutoRunTime;
     
     private Map<String, String> clientInfo;

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
    
    public Map<String, String> getClientInfo() {
        return clientInfo;
    }

    public void setClientInfo(Map<String, String> clientInfo) {
        this.clientInfo = clientInfo;
    }

    @Override
    public String toString() {
        return "AsyncCallRealRes [result=" + result + ", responseContent=" + responseContent + ", variables="
                + variables + ", autoRunFlag=" + autoRunFlag + ", nextAutoRunTime=" + nextAutoRunTime + 
                 ", clientInfo=" + clientInfo + "]";
    }



}
