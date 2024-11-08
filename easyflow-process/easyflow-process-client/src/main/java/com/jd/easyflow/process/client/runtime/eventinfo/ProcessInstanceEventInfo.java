package com.jd.easyflow.process.client.runtime.eventinfo;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessInstanceEventInfo {

    private ProcessInstanceDTO processInstance;
    
    private String event;

    public ProcessInstanceDTO getProcessInstance() {
        return processInstance;
    }

    public void setProcessInstance(ProcessInstanceDTO processInstance) {
        this.processInstance = processInstance;
    }

    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }
    
    
}
