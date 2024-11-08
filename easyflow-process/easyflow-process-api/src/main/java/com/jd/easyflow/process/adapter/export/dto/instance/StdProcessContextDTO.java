package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class StdProcessContextDTO implements Serializable {
    
    private Map<String, Object> processProperties;
    
    private StdProcessDTO process;

    public Map<String, Object> getProcessProperties() {
        return processProperties;
    }

    public void setProcessProperties(Map<String, Object> processProperties) {
        this.processProperties = processProperties;
    }

    public StdProcessDTO getProcess() {
        return process;
    }

    public void setProcess(StdProcessDTO process) {
        this.process = process;
    }

    @Override
    public String toString() {
        return "StdProcessContextDTO [processProperties=" + processProperties + ", process=" + process + "]";
    }
    
    
}
