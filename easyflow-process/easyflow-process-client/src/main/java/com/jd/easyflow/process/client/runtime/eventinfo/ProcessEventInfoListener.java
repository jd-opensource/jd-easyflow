package com.jd.easyflow.process.client.runtime.eventinfo;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessEventInfoListener {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessEventInfoListener.class);


    public void onProcessInstanceStart(ProcessInstanceDTO processInstance, ProcessEventInfoCache eventCache) {
        if (log.isDebugEnabled()) {
            log.debug("process instance start." + processInstance);
        }
        ProcessInstanceEventInfo event = new ProcessInstanceEventInfo();
        event.setEvent(StdProcessConstants.EVENT_PROCESS_INSTANCE_START);
        event.setProcessInstance(processInstance);
        eventCache.addUnflushProcessInstanceEventInfo(event);
    }

    public void onProcessInstanceEnd(ProcessInstanceDTO processInstance, ProcessEventInfoCache eventCache) {
        if (log.isDebugEnabled()) {
            log.debug("process instance end." + processInstance);
        }
        ProcessInstanceEventInfo event = new ProcessInstanceEventInfo();
        event.setEvent(StdProcessConstants.EVENT_PROCESS_INSTANCE_END);
        event.setProcessInstance(processInstance);
        eventCache.addUnflushProcessInstanceEventInfo(event);
    }

    public void onNodeInstanceStart(Object[] eventData, ProcessEventInfoCache eventCache) {
        if (log.isDebugEnabled()) {
            log.debug("node instance start." + eventData);
        }
        ProcessNodeInstanceDTO nodeInstance = (ProcessNodeInstanceDTO) eventData[0];
        StdNodeContext nodeContext = (StdNodeContext) eventData[1];
        NodeInstanceEventInfo event = new NodeInstanceEventInfo();
        event.setEvent(StdProcessConstants.EVENT_NODE_INSTANCE_START);
        event.setNodeInstance(nodeInstance);
        event.setStdNodeContext(nodeContext);
        eventCache.addUnflushNodeInstanceEventInfo(event);
    }

    public void onNodeInstanceEnd(Object[] eventData, ProcessEventInfoCache eventCache) {
        if (log.isDebugEnabled()) {
            log.debug("node instance end." + eventData);
        }
        ProcessNodeInstanceDTO nodeInstance = (ProcessNodeInstanceDTO) eventData[0];
        StdNodeContext nodeContext = (StdNodeContext) eventData[1];
        NodeInstanceEventInfo event = new NodeInstanceEventInfo();
        event.setEvent(StdProcessConstants.EVENT_NODE_INSTANCE_END);
        event.setNodeInstance(nodeInstance);
        event.setStdNodeContext(nodeContext);;
        eventCache.addUnflushNodeInstanceEventInfo(event);
    }

    public void onTxnFlushStart(ProcessEventInfoCache eventCache) {

    }

    public void onTxnFlushEnd(ProcessEventInfoCache eventCache) {
        eventCache.eventFlush();
    }

}
