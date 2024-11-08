package com.jd.easyflow.process.spi.client.dto;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class ProcessExecuteReq implements Serializable {

    private String processId;
    
    private String[] nodeIds;
    
    private Object param;
    
    private Map<String, Object> dataMap;

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

    @Override
    public String toString() {
        return "ProcessExecuteReq [processId=" + processId + ", nodeIds=" + Arrays.toString(nodeIds) + ", param="
                + param + ", dataMap=" + dataMap + "]";
    }
    
}
