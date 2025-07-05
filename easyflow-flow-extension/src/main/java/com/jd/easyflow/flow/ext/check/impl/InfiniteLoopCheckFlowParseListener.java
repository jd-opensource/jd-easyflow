package com.jd.easyflow.flow.ext.check.impl;

import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;
import com.jd.easyflow.flow.util.FlowNodeLinkUtil;

/**
 * @author liyuliang5
 */
public class InfiniteLoopCheckFlowParseListener implements FlowParseEventListener {

    private static final Logger logger = LoggerFactory.getLogger(InfiniteLoopCheckFlowParseListener.class);
    
    private Set<String> flowWhitelist;


    @Override
    public void on(FlowParseEvent event) {
        switch (event.getType()) {
        case FlowParseEventTypes.INIT_FLOW_END: {
            if (logger.isDebugEnabled()) {
                logger.debug("Start flow infinite loop check, flow ID:" + event.getFlow().getId());
            }
            if (flowWhitelist != null && flowWhitelist.contains(event.getFlow().getId())) {
                return;
            }
            Flow flow = event.getFlow();
            for (FlowNode node : flow.getNodeList()) {
                boolean infiniteLoop = FlowNodeLinkUtil.isReachable(node.getId(), node.getId(), flow);
                if (infiniteLoop) {
                    throw new FlowException("flow:" + flow.getId() + " node:" + node.getId() + " is infinite loop node!");
                }
            }
        }

        }
    }


    public Set<String> getFlowWhitelist() {
        return flowWhitelist;
    }


    public void setFlowWhitelist(Set<String> flowWhitelist) {
        this.flowWhitelist = flowWhitelist;
    }
    
}
