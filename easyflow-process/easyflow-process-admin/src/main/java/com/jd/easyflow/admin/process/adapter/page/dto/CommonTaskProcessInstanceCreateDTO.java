package com.jd.easyflow.admin.process.adapter.page.dto;

/**
 * @author liyuliang5
 *
 */
public class CommonTaskProcessInstanceCreateDTO {

    private String processId;
    private String instanceName;
    private String productCode;
    private String instanceBizData;
    private String keyField;
    private String keyField2;
    private String taskData;
    public String getProcessId() {
        return processId;
    }
    public void setProcessId(String processId) {
        this.processId = processId;
    }
    public String getInstanceName() {
        return instanceName;
    }
    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
    }
    public String getProductCode() {
        return productCode;
    }
    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }
    public String getInstanceBizData() {
        return instanceBizData;
    }
    public void setInstanceBizData(String instanceBizData) {
        this.instanceBizData = instanceBizData;
    }
    public String getKeyField() {
        return keyField;
    }
    public void setKeyField(String keyField) {
        this.keyField = keyField;
    }
    public String getKeyField2() {
        return keyField2;
    }
    public void setKeyField2(String keyField2) {
        this.keyField2 = keyField2;
    }
    public String getTaskData() {
        return taskData;
    }
    public void setTaskData(String taskData) {
        this.taskData = taskData;
    }
    @Override
    public String toString() {
        return "CommonTaskProcessInstanceCreateDTO [processId=" + processId + ", instanceName=" + instanceName
                + ", productCode=" + productCode + ", instanceBizData=" + instanceBizData + ", keyField=" + keyField
                + ", keyField2=" + keyField2 + ", taskData=" + taskData + "]";
    }
    
    
}
