package com.jd.easyflow.process.client.runtime.eventinfo;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.client.runtime.StdNodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class NodeInstanceEventInfo {

    private ProcessNodeInstanceDTO nodeInstance;
    
    private StdNodeContext stdNodeContext;
    
    private String event;


    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }

    public ProcessNodeInstanceDTO getNodeInstance() {
        return nodeInstance;
    }

    public void setNodeInstance(ProcessNodeInstanceDTO nodeInstance) {
        this.nodeInstance = nodeInstance;
    }

    public StdNodeContext getStdNodeContext() {
        return stdNodeContext;
    }

    public void setStdNodeContext(StdNodeContext stdNodeContext) {
        this.stdNodeContext = stdNodeContext;
    }
    
}
