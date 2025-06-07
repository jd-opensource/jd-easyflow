package com.jd.easyflow.flow.engine.event;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.util.Pair;

/**
 * @author liyuliang5
 */
public abstract class BaseFlowEventListener implements FlowEventListener {
    
    protected Pair<String, Integer>[] acceptedEvents;

    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        return acceptedEvents;
    }
    
    public void setAcceptedEvents(Pair<String, Integer>[] acceptedEvents) {
        this.acceptedEvents = acceptedEvents;
    }

    @Override
    public void postConstruct(Map<String, Object> definition, Map<String, Object> context) {
        if (definition == null) {
            return;
        }
        List<Map<String, Object>> acceptedEvents = (List<Map<String, Object>>)definition.get("acceptedEvents");
        if (acceptedEvents != null) {
            this.acceptedEvents = new Pair[acceptedEvents.size()];
            for (int i = 0; i < acceptedEvents.size(); i++) {
                Map<String, Object> acceptedEvent = acceptedEvents.get(i);
                String event = (String) acceptedEvent.get("event");
                Integer order = (Integer) acceptedEvent.get("order");
                this.acceptedEvents[i] = Pair.of(event, order);
            }
        }
    }

}
