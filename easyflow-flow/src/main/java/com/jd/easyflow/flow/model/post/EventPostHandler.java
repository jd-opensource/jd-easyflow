package com.jd.easyflow.flow.model.post;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.engine.event.impl.EventFlowListener;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.parser.param.PostParseParam;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class EventPostHandler implements NodePostHandler {

    private static final Logger logger = LoggerFactory.getLogger(EventPostHandler.class);
    
    private boolean autoAddEventFlowListener = true;
    
    public EventPostHandler() {
    }

    public EventPostHandler(boolean autoAddEventFlowListener) {
        this.autoAddEventFlowListener = autoAddEventFlowListener;
    }
    
    @Override
    public void init(InitContext initContext, Object flowNode) {
        initEventPostHandlerMap(initContext, (FlowNode) flowNode);
        if (autoAddEventFlowListener) {
            List<FlowEventListener> listeners = initContext.getFlow().getEventTrigger().getListenerList();
            boolean exists = false;
            if (listeners != null) {
                for (FlowEventListener listener : listeners) {
                    if (listener instanceof EventFlowListener) {
                        exists = true;
                        break;
                    }
                }
            }
            if (! exists) {
                logger.info("Auto add EventFlowListener");
                initContext.getFlow().getEventTrigger().addListener(new EventFlowListener());
            }
        }
    }

    @Override
    public NodeContext[] postHandle(NodeContext nodeContext, FlowContext context) {
        String event = nodeContext.get(FlowConstants.NODE_CONTEXT_DATA_EVENT);
        if (context.isLogOn() && logger.isInfoEnabled()) {
            logger.info("Event:" + event);
        }
        if (event == null) {
            event = FlowConstants.NONE_EVENT;
        }

        Map<String, NodePostHandler> eventPostHandlerMap = context.getFlow().getNode(nodeContext.getNodeId())
                .getProperty(FlowConstants.PROP_RUNTIME_EVENT_POST_HANDLER_MAP);
        NodePostHandler postHandler = eventPostHandlerMap.get(event);
        if (postHandler == null) {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Event post handler is null");
            }
            return null;
        }
        return postHandler.postHandle(nodeContext, context);
    }

    private void initEventPostHandlerMap(InitContext initContext, FlowNode flowNode) {
        Map<String, Object> eventPostHandlerMap = flowNode
                .getProperty(FlowConstants.PROP_RUNTIME_EVENT_POST_HANDLER_MAP);
        if (eventPostHandlerMap != null) {
            return;
        }
        eventPostHandlerMap = new ConcurrentHashMap<String, Object>();
        flowNode.setProperty(FlowConstants.PROP_RUNTIME_EVENT_POST_HANDLER_MAP, eventPostHandlerMap);
        Map<String, Object> map = flowNode.getProperty(FlowConstants.NODE_PROP_EVENTS);
        if (map == null) {
            return;
        }
        for (Entry<String, Object> entry : map.entrySet()) {
            String event = entry.getKey();
            Object eventConf = (Object) entry.getValue();
            Map<String, Object> eventPostHandlerConfMap = null;
            if (eventConf instanceof Map) {
                Map<String, Object> eventConfMap = (Map<String, Object>) eventConf;
                eventPostHandlerConfMap = (Map<String, Object>) eventConfMap.get("post");
            }
            if (eventPostHandlerConfMap != null) {
                NodePostHandler nodePostHandler = initContext.getFlowParser()
                        .parseNodePostHandler(new PostParseParam(eventPostHandlerConfMap, initContext.getFlowList(),
                                initContext.isParseEl(), initContext.getFlow(), flowNode));
                eventPostHandlerMap.put(event, nodePostHandler);
            }
        }

    }

    public boolean isAutoAddEventFlowListener() {
        return autoAddEventFlowListener;
    }

    public void setAutoAddEventFlowListener(boolean autoAddEventFlowListener) {
        this.autoAddEventFlowListener = autoAddEventFlowListener;
    }
    
    

}
