package com.jd.easyflow.process.domain.model.vo;

import java.util.Arrays;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class ScheduleProcessReqVO {

    private String processId;
    
    private String[] nodeIds;
    
    private Object param;
    
    private Map<String, Object> dataMap;
    
    private String processType;
    
    private String bizNo;
    
    private String processInstanceNo;
    
    private String productCode;

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

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    @Override
    public String toString() {
        return "ScheduleProcessReqVO [processId=" + processId + ", nodeIds=" + Arrays.toString(nodeIds) + ", param="
                + param + ", dataMap=" + dataMap + ", processType=" + processType + ", bizNo=" + bizNo
                + ", processInstanceNo=" + processInstanceNo + ", productCode=" + productCode + "]";
    }
    
    
    
}
