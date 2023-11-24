package com.jd.easyflow.flow.model.parser.event;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FlowParseEventListener {
    
    public void on(FlowParseEvent event);
    
    default void postConstruct(Map<String, Object> definition, Map<String, Object> context) {}

}
