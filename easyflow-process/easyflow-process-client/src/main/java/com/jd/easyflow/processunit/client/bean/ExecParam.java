package com.jd.easyflow.processunit.client.bean;

import java.util.Map;
import java.util.function.Function;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * @author liyuliang5
 *
 */
public class ExecParam {

    private String unitCode;
    
    private String bizNo;
    
    private String requestNo;
    private Function<ExecContext, ?> invoker;
    private Function<ExecContext, String> resultFunction;
    private Function<ExecContext, String> responseContentFunction;
    private Function<ExecContext, ?> oldResultFunction;
    
    private String instanceNo;
    
    private String executeExp;

    private String requestContent;

    private Map<String, Object> requestContext;
    
    private String executionNo;
    
    private String productCode;
    
    private String parentNo;
    
    @JsonIgnore
    private ExecContext execContext;
    
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

    public String getRequestNo() {
        return requestNo;
    }

    public void setRequestNo(String requestNo) {
        this.requestNo = requestNo;
    }

    public Function<ExecContext, ?> getInvoker() {
        return invoker;
    }

    public void setInvoker(Function<ExecContext, ?> invoker) {
        this.invoker = invoker;
    }

    public Function<ExecContext, String> getResultFunction() {
        return resultFunction;
    }

    public void setResultFunction(Function<ExecContext, String> resultFunction) {
        this.resultFunction = resultFunction;
    }

    public Function<ExecContext, String> getResponseContentFunction() {
        return responseContentFunction;
    }

    public void setResponseContentFunction(Function<ExecContext, String> responseContentFunction) {
        this.responseContentFunction = responseContentFunction;
    }

    public Function<ExecContext, ?> getOldResultFunction() {
        return oldResultFunction;
    }

    public void setOldResultFunction(Function<ExecContext, ?> oldResultFunction) {
        this.oldResultFunction = oldResultFunction;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    public String getExecuteExp() {
        return executeExp;
    }

    public void setExecuteExp(String executeExp) {
        this.executeExp = executeExp;
    }

    public String getRequestContent() {
        return requestContent;
    }

    public void setRequestContent(String requestContent) {
        this.requestContent = requestContent;
    }

    public Map<String, Object> getRequestContext() {
        return requestContext;
    }

    public void setRequestContext(Map<String, Object> requestContext) {
        this.requestContext = requestContext;
    }

    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getParentNo() {
        return parentNo;
    }

    public void setParentNo(String parentNo) {
        this.parentNo = parentNo;
    }

    public ExecContext getExecContext() {
        return execContext;
    }

    public void setExecContext(ExecContext execContext) {
        this.execContext = execContext;
    }

    @Override
    public String toString() {
        return "ExecParam [unitCode=" + unitCode + ", bizNo=" + bizNo + ", requestNo=" + requestNo + ", instanceNo="
                + instanceNo + ", executeExp=" + executeExp + ", requestContent=" + requestContent + ", requestContext="
                + requestContext + ", executionNo=" + executionNo + ", productCode=" + productCode + ", parentNo="
                + parentNo + "]";
    }


    
    
}
