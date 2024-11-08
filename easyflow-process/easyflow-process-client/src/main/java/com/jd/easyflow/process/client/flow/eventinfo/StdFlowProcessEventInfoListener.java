package com.jd.easyflow.process.client.flow.eventinfo;

import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.process.client.runtime.eventinfo.ProcessEventInfoCache;
import com.jd.easyflow.process.client.runtime.eventinfo.ProcessEventInfoListener;

/**
 * 
 * @author liyuliang5
 *
 */
public class StdFlowProcessEventInfoListener implements FlowEventListener {

    private ProcessEventInfoListener processEventInfoListener;

    private static final Pair<String, Integer>[] DEFAULT_ACCEPTED_EVENTS = new Pair[] {
            Pair.of(StdProcessConstants.EVENT_PROCESS_INSTANCE_START, FlowConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_PROCESS_INSTANCE_END, -FlowConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_NODE_INSTANCE_START, FlowConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_NODE_INSTANCE_END, -FlowConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_TXN_FLUSH_START, FlowConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_TXN_FLUSH_END, -FlowConstants.EVENT_ORDER_START) };

    private Pair<String, Integer>[] acceptedEvents = DEFAULT_ACCEPTED_EVENTS;
    
    public StdFlowProcessEventInfoListener() {
        this.processEventInfoListener = new ProcessEventInfoListener();
        
    }
    
    public StdFlowProcessEventInfoListener(ProcessEventInfoListener processEventInfoListener) {
        this.processEventInfoListener = processEventInfoListener;
    }

    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        return acceptedEvents;
    }

    @Override
    public void on(FlowEvent flowEvent) {
        switch (flowEvent.getType()) {
        case StdProcessConstants.EVENT_PROCESS_INSTANCE_START: {
            ProcessEventInfoCache cache = getEventInfoCache(flowEvent);
            processEventInfoListener.onProcessInstanceStart((ProcessInstanceDTO) flowEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_PROCESS_INSTANCE_END: {
            ProcessEventInfoCache cache = getEventInfoCache(flowEvent);
            processEventInfoListener.onProcessInstanceEnd((ProcessInstanceDTO) flowEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_NODE_INSTANCE_START: {
            ProcessEventInfoCache cache = getEventInfoCache(flowEvent);
            processEventInfoListener.onNodeInstanceStart((Object[]) flowEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_NODE_INSTANCE_END: {
            ProcessEventInfoCache cache = getEventInfoCache(flowEvent);
            processEventInfoListener.onNodeInstanceEnd((Object[]) flowEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_TXN_FLUSH_START: {
            ProcessEventInfoCache cache = getEventInfoCache(flowEvent);
            processEventInfoListener.onTxnFlushStart(cache);
            break;
        }
        case StdProcessConstants.EVENT_TXN_FLUSH_END: {
            ProcessEventInfoCache container = getEventInfoCache(flowEvent);
            processEventInfoListener.onTxnFlushEnd(container);
            break;
        }
        default:
            break;
        }

    }

    private ProcessEventInfoCache getEventInfoCache(FlowEvent event) {
        ProcessEventInfoCache cache = event.getContext().get(StdFlowProcessConstants.FLOW_CTX_PROCESS_EVENT_INFO_CACHE);
        if (cache == null) {
            synchronized (event.getContext()) {
                cache = event.getContext().get(StdFlowProcessConstants.FLOW_CTX_PROCESS_EVENT_INFO_CACHE);
                if (cache == null) {
                    cache = new ProcessEventInfoCache();
                    event.getContext().put(StdFlowProcessConstants.FLOW_CTX_PROCESS_EVENT_INFO_CACHE, cache);
                }
            }

        }
        return cache;
    }

    public ProcessEventInfoListener getNodeInstanceStatusListener() {
        return processEventInfoListener;
    }

    public void setNodeInstanceStatusListener(ProcessEventInfoListener nodeInstanceStatusListener) {
        this.processEventInfoListener = nodeInstanceStatusListener;
    }

    public void setAcceptedEvents(Pair<String, Integer>[] acceptedEvents) {
        this.acceptedEvents = acceptedEvents;
    }

}
