package com.jd.easyflow.process.client.fsm.eventinfo;

import com.jd.easyflow.fsm.event.FsmEvent;
import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.util.FsmConstants;
import com.jd.easyflow.fsm.util.Pair;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.client.fsm.StdFsmProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.process.client.runtime.eventinfo.ProcessEventInfoCache;
import com.jd.easyflow.process.client.runtime.eventinfo.ProcessEventInfoListener;

/**
 * 
 * @author liyuliang5
 *
 */
public class StdFsmProcessEventInfoListener implements FsmEventListener {

    private ProcessEventInfoListener processEventInfoListener;

    private static final Pair<String, Integer>[] DEFAULT_ACCEPTED_EVENTS = new Pair[] {
            Pair.of(StdProcessConstants.EVENT_PROCESS_INSTANCE_START, FsmConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_PROCESS_INSTANCE_END, -FsmConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_NODE_INSTANCE_START, FsmConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_NODE_INSTANCE_END, -FsmConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_TXN_FLUSH_START, FsmConstants.EVENT_ORDER_START),
            Pair.of(StdProcessConstants.EVENT_TXN_FLUSH_END, -FsmConstants.EVENT_ORDER_START) };

    private Pair<String, Integer>[] acceptedEvents = DEFAULT_ACCEPTED_EVENTS;
    
    public StdFsmProcessEventInfoListener() {
        this.processEventInfoListener = new ProcessEventInfoListener();
    }
    
    public StdFsmProcessEventInfoListener(ProcessEventInfoListener processEventInfoListener) {
        this.processEventInfoListener = processEventInfoListener;
    }

    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        return acceptedEvents;
    }

    @Override
    public void on(FsmEvent fsmEvent) {
        switch (fsmEvent.getType()) {
        case StdProcessConstants.EVENT_PROCESS_INSTANCE_START: {
            ProcessEventInfoCache cache = getEventInfoCache(fsmEvent);
            processEventInfoListener.onProcessInstanceStart((ProcessInstanceDTO) fsmEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_PROCESS_INSTANCE_END: {
            ProcessEventInfoCache cache = getEventInfoCache(fsmEvent);
            processEventInfoListener.onProcessInstanceEnd((ProcessInstanceDTO) fsmEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_NODE_INSTANCE_START: {
            ProcessEventInfoCache cache = getEventInfoCache(fsmEvent);
            processEventInfoListener.onNodeInstanceStart((Object[]) fsmEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_NODE_INSTANCE_END: {
            ProcessEventInfoCache cache = getEventInfoCache(fsmEvent);
            processEventInfoListener.onNodeInstanceEnd((Object[]) fsmEvent.getData(), cache);
            break;
        }
        case StdProcessConstants.EVENT_TXN_FLUSH_START: {
            ProcessEventInfoCache cache = getEventInfoCache(fsmEvent);
            processEventInfoListener.onTxnFlushStart(cache);
            break;
        }
        case StdProcessConstants.EVENT_TXN_FLUSH_END: {
            ProcessEventInfoCache container = getEventInfoCache(fsmEvent);
            processEventInfoListener.onTxnFlushEnd(container);
            break;
        }
        default:
            break;
        }

    }

    private ProcessEventInfoCache getEventInfoCache(FsmEvent event) {
        ProcessEventInfoCache cache = event.getContext().getData(StdFsmProcessConstants.FSM_CTX_PROCESS_EVENT_INFO_CACHE);
        if (cache == null) {
            synchronized (event.getContext()) {
                cache = event.getContext().getData(StdFsmProcessConstants.FSM_CTX_PROCESS_EVENT_INFO_CACHE);
                if (cache == null) {
                    cache = new ProcessEventInfoCache();
                    event.getContext().putData(StdFsmProcessConstants.FSM_CTX_PROCESS_EVENT_INFO_CACHE, cache);
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

