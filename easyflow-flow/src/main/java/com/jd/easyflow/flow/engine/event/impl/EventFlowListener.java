package com.jd.easyflow.flow.engine.event.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.event.BaseFlowEventListener;
import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.FlowStringUtil;
import com.jd.easyflow.flow.util.Pair;

/**
 * Listener of event node.
 * @author liyuliang5
 *
 */
public class EventFlowListener extends BaseFlowEventListener {

    private static final Logger logger = LoggerFactory.getLogger(EventFlowListener.class);
    
    private int initEndEventOrder = FlowConstants.EVENT_ORDER_START;

    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        if (this.acceptedEvents != null) {
            return acceptedEvents;
        }
        return new Pair[] { Pair.of(FlowEventTypes.INIT_END, initEndEventOrder) };
    }

    @Override
    public void on(FlowEvent event) {
        switch (event.getType()) {
        case FlowEventTypes.INIT_END: {
            String eventId = event.getContext().getParam().get(FlowConstants.PARAM_DATA_EVENT);
            if (FlowStringUtil.isNotEmpty(eventId)) {
                List<NodeContext> startNodes = event.getContext().getStartNodes();
                if (event.getContext().isLogOn() && logger.isInfoEnabled()) {
                    logger.info("EVENT ID:" + eventId);
                }
                if (startNodes != null && ! startNodes.isEmpty()) {
                    startNodes.forEach(node -> node.put(FlowConstants.NODE_CONTEXT_DATA_EVENT, eventId));
                }
            }
            break;
        }
        default:
            break;
        }

    }

    public int getInitEndEventOrder() {
        return initEndEventOrder;
    }

    public void setInitEndEventOrder(int initEndEventOrder) {
        this.initEndEventOrder = initEndEventOrder;
    }
    
    

}
