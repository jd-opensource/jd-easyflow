package com.jd.easyflow.process.client.task.flow;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcess;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class TaskFlowListener implements FlowEventListener {

    private static final Pair<String, Integer>[] DEFAULT_ACCEPTED_EVENTS = new Pair[] {
            Pair.of(FlowEventTypes.FLOW_START, FlowConstants.EVENT_ORDER_START + 90),
            Pair.of(FlowEventTypes.NODE_START, FlowConstants.EVENT_ORDER_START + 100) };

    private Pair<String, Integer>[] acceptedEvents = DEFAULT_ACCEPTED_EVENTS;

    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        return acceptedEvents;
    }

    @Override
    public void on(FlowEvent flowEvent) {
        switch (flowEvent.getType()) {
        case FlowEventTypes.FLOW_START: {
            onFlowStart(flowEvent);
            break;
        }
        case FlowEventTypes.NODE_START: {
            onNodeStart(flowEvent);
            break;
        }
        default:
            break;
        }

    }

    public void onFlowStart(FlowEvent event) {
        FlowContext context = event.getContext();
        StdProcessContext processContext = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
        StdProcess process = (StdProcess) processContext.getProcess();
        process.putExtProperty(StdProcessConstants.PROP_TASK,
                event.getContext().getFlow().getProperty(StdProcessConstants.PROP_TASK));
    }

    public void onNodeStart(FlowEvent event) {

    }

}
