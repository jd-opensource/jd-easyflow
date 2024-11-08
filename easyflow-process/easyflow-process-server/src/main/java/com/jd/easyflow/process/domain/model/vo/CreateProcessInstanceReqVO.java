package com.jd.easyflow.process.domain.model.vo;

import java.util.Arrays;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class CreateProcessInstanceReqVO {
    /**
     * common|std
     */
    private String type;
    
    /**
     * required.
     */
    private String processId;
    
    /**
     * optional.
     * Start node id list.
     */
    private String[] nodeIds;
    
    /**
     * optional.
     * Json format.
     */
    private Object param;
    
    /**
     * optional.
     * CollectionType.
     */
    private Map<String, Object> dataMap;

    /**
     * required.
     */
    private String processType;
    /**
     * required.
     */
    private String bizNo;
    
    /**
     */
    private String bizStatus;
    
    /**
     * JSON Format. optional.
     */
    private String bizData;
    
    /**
     * optional
     */
    private String creator;
    /**
     * optional
     */
    private String productCode;
    /**
     * optional
     */
    private String keyField;
    /**
     * optional
     */
    private String keyField2;
    /**
     * optional
     */
    private String instanceName;
    public String getType() {
        return type;
    }
    public void setType(String type) {
        this.type = type;
    }
    public String getProcessId() {
        return processId;
    }
    public void setProcessId(String processId) {
        this.processId = processId;
    }
    public String[] getNodeIds() {
        return nodeIds;
    }
    public void setNodeIds(String[] nodeIds) {
        this.nodeIds = nodeIds;
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
    public String getInstanceName() {
        return instanceName;
    }
    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
    }
    @Override
    public String toString() {
        return "CreateProcessInstanceReqVO [type=" + type + ", processId=" + processId + ", nodeIds="
                + Arrays.toString(nodeIds) + ", param=" + param + ", dataMap=" + dataMap + ", processType="
                + processType + ", bizNo=" + bizNo + ", bizStatus=" + bizStatus + ", bizData=" + bizData + ", creator="
                + creator + ", productCode=" + productCode + ", keyField=" + keyField + ", keyField2=" + keyField2
                + ", instanceName=" + instanceName + "]";
    }
    
    
    
}
