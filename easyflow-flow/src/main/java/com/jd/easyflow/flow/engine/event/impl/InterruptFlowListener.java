package com.jd.easyflow.flow.engine.event.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.FlowUtil;
import com.jd.easyflow.flow.util.Pair;

/**
 * Listen node end event, process interrupt scene.
 * @author liyuliang5
 *
 */
public class InterruptFlowListener implements FlowEventListener {

    private static final Logger logger = LoggerFactory.getLogger(InterruptFlowListener.class);
    
    private int nodeEndEventOrder = FlowConstants.EVENT_ORDER_START;

    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        return new Pair[] { Pair.of(FlowEventTypes.NODE_END, nodeEndEventOrder) };
    }

    @Override
    public void on(FlowEvent event) {
        switch (event.getType()) {
        case FlowEventTypes.NODE_END: {
            NodeContext nodeContext = (NodeContext) event.getData();
            FlowContext context = event.getContext();
            Boolean interrupt = FlowUtil.nodeProperty(FlowConstants.PROP_INTERRUPT, nodeContext, context);
            if (interrupt == null) {
                String interruptExp = FlowUtil.nodeProperty(FlowConstants.PROP_INTERRUPT_EXP, nodeContext, context);
                if (interruptExp != null) {
                    interrupt = context.getElEvaluator().eval(interruptExp, nodeContext, context, null);
                }
                if (interrupt == null) {
                    String flowInterruptExp = context.getFlow().getProperty(FlowConstants.PROP_INTERRUPT_EXP);
                    if (flowInterruptExp != null) {
                        interrupt = context.getElEvaluator().eval(flowInterruptExp, nodeContext, context, null);
                    }
                }
            }
            if (logger.isDebugEnabled()) {
                logger.debug("Flow interrupt result:" + interrupt);
            }
            if (Boolean.TRUE.equals(interrupt)) {
                context.setInterrupted();
            }
            break;
        }
        default:
            break;
        }

    }

    public int getNodeEndEventOrder() {
        return nodeEndEventOrder;
    }

    public void setNodeEndEventOrder(int nodeEndEventOrder) {
        this.nodeEndEventOrder = nodeEndEventOrder;
    }
    
}
