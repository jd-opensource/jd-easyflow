package com.jd.easyflow.process.client.task.service.dto;

import java.util.Map;
import java.util.function.Function;

import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * @author liyuliang5
 *
 */
public class TaskCreateParam {
    
    private String user;

    private String productCode;
    
    private String processType;
    
    private String bizNo;
    
    private String taskBizCode;
    
    private String taskBizName;
    
    private Map<String, Object> taskProperties;
    
    private Map<String, Object> flowTaskProperties;
    
    private StdProcessContext processContext; 
    
    private StdNodeContext nodeContext;
    
    private Function<String, Object> elFunction;
    
    private String instanceBizStatus;
    
    private String instanceBizData;

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getProcessType() {
        return processType;
    }

    public void setProcessType(String processType) {
        this.processType = processType;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getTaskBizCode() {
        return taskBizCode;
    }

    public void setTaskBizCode(String taskBizCode) {
        this.taskBizCode = taskBizCode;
    }

    public String getTaskBizName() {
        return taskBizName;
    }

    public void setTaskBizName(String taskBizName) {
        this.taskBizName = taskBizName;
    }

    public Map<String, Object> getTaskProperties() {
        return taskProperties;
    }

    public void setTaskProperties(Map<String, Object> taskProperties) {
        this.taskProperties = taskProperties;
    }

    public Map<String, Object> getFlowTaskProperties() {
        return flowTaskProperties;
    }

    public void setFlowTaskProperties(Map<String, Object> flowTaskProperties) {
        this.flowTaskProperties = flowTaskProperties;
    }

    public StdProcessContext getProcessContext() {
        return processContext;
    }

    public void setProcessContext(StdProcessContext processContext) {
        this.processContext = processContext;
    }

    public StdNodeContext getNodeContext() {
        return nodeContext;
    }

    public void setNodeContext(StdNodeContext nodeContext) {
        this.nodeContext = nodeContext;
    }

    public Function<String, Object> getElFunction() {
        return elFunction;
    }

    public void setElFunction(Function<String, Object> elFunction) {
        this.elFunction = elFunction;
    }

    public String getInstanceBizStatus() {
        return instanceBizStatus;
    }

    public void setInstanceBizStatus(String instanceBizStatus) {
        this.instanceBizStatus = instanceBizStatus;
    }

    public String getInstanceBizData() {
        return instanceBizData;
    }

    public void setInstanceBizData(String instanceBizData) {
        this.instanceBizData = instanceBizData;
    }

    @Override
    public String toString() {
        return "TaskCreateParam [user=" + user + ", productCode=" + productCode + ", processType=" + processType
                + ", bizNo=" + bizNo + ", taskBizCode=" + taskBizCode + ", taskBizName=" + taskBizName
                + ", taskProperties=" + taskProperties + ", flowTaskProperties=" + flowTaskProperties
                + ", processContext=" + processContext + ", nodeContext=" + nodeContext + ", elFunction=" + elFunction
                + ", instanceBizStatus=" + instanceBizStatus + ", instanceBizData=" + instanceBizData + "]";
    }
    
    
}
