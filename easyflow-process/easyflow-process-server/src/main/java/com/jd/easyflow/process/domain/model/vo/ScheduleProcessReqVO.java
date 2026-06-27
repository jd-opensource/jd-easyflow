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
        return "ScheduleProcessReqVO [processId=" + processId + ", nodeIds=" + Arrays.toString(nodeIds) + ", param="
                + param + ", dataMap=" + dataMap + "]";
    }
    
    
    
}
