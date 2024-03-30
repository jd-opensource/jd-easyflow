package com.jd.easyflow.flow.engine.event;

import com.jd.easyflow.flow.model.FlowLifeCycle;
import com.jd.easyflow.flow.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FlowEventListener extends FlowLifeCycle {
	
    /**
     * 
     * @return List of Event and priority pair.
     */
    default Pair<String, Integer>[] getAcceptedEvents() {
        return null;
    }
    
    /**
     * Listener Id.
     * @return
     */
    default String getId() {
        return this.getClass().getName();
    }
    
    /**
     * Callback of event.
     * @param flowEvent
     */
    public void on(FlowEvent flowEvent);
    

}
