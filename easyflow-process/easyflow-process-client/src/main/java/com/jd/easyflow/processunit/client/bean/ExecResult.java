package com.jd.easyflow.processunit.client.bean;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * @author liyuliang5
 *
 */
public class ExecResult {
    
    public ExecResult() {
        
    }
    
    public ExecResult(String result, String responseContent) {
        this.result = result;
        this.responseContent = responseContent;
    }
    
    public ExecResult(String result, String responseContent, Object execResult, Throwable execException, String instanceNo, ExecContext execContext) {
        this.result = result;
        this.responseContent = responseContent;
        this.execResult = execResult;
        this.execException = execException;
        this.instanceNo = instanceNo;
        this.execContext = execContext;
    }
    
    private String result;
    
    private String responseContent;
    
    private Object execResult;
    
    private Throwable execException;
    
    private String instanceNo;
    
    @JsonIgnore
    private ExecContext execContext;
    
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

    public Object getExecResult() {
        return execResult;
    }

    public void setExecResult(Object execResult) {
        this.execResult = execResult;
    }

    public Throwable getExecException() {
        return execException;
    }

    public void setExecException(Throwable execException) {
        this.execException = execException;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    public ExecContext getExecContext() {
        return execContext;
    }

    public void setExecContext(ExecContext execContext) {
        this.execContext = execContext;
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
        return "ExecResult [result=" + result + ", responseContent=" + responseContent + ", execResult=" + execResult
                + ", execException=" + execException + ", instanceNo=" + instanceNo + ", autoRunFlag=" + autoRunFlag
                + ", nextAutoRunTime=" + nextAutoRunTime + "]";
    }




}
