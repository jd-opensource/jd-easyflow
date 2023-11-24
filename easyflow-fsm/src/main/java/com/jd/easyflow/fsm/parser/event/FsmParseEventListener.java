package com.jd.easyflow.fsm.parser.event;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FsmParseEventListener {
    
    public void on(FsmParseEvent event);
    
    default void postConstruct(Map<String, Object> definition, Map<String, Object> context) {}

}
