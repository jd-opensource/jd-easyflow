package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class CreateProcessInstanceReq implements Serializable {


    private String processType;

    private String bizNo;

    private String processId;

    private String instanceName;

    private Object param;

    private Map<String, Object> dataMap;

    private String bizStatus;

    private String bizData;

    private String creator;

    private String productCode;

    private String keyField;

    private String keyField2;
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
    public Object getParam() {
        return param;
    }
    public void setParam(Object param) {
        this.param = param;
    }
    public Map<String, Object> getDataMap() {
        return dataMap;
    }
    public void setDataMap(Map<String, Object> dataMap) {
        this.dataMap = dataMap;
    }
    public String getBizStatus() {
        return bizStatus;
    }
    public void setBizStatus(String bizStatus) {
        this.bizStatus = bizStatus;
    }
    public String getBizData() {
        return bizData;
    }
    public void setBizData(String bizData) {
        this.bizData = bizData;
    }
    public String getCreator() {
        return creator;
    }
    public void setCreator(String creator) {
        this.creator = creator;
    }
    public String getProductCode() {
        return productCode;
    }
    public void setProductCode(String productCode) {
        this.productCode = productCode;
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
    @Override
    public String toString() {
        return "CreateProcessInstanceReq [processType=" + processType + ", bizNo=" + bizNo + ", processId=" + processId
                + ", instanceName=" + instanceName + ", param=" + param + ", dataMap=" + dataMap + ", bizStatus="
                + bizStatus + ", bizData=" + bizData + ", creator=" + creator + ", productCode=" + productCode
                + ", keyField=" + keyField + ", keyField2=" + keyField2 + "]";
    }
    
    
    
}
