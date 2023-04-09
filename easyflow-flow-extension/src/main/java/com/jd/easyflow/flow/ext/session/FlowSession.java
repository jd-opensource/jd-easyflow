package com.jd.easyflow.flow.ext.session;

/**
 * Session that cross flow.
 * 
 * @author liyuliang5
 *
 */
public interface FlowSession {
    
    String getId();

    void put(String key, Object value);

    <T> T get(String key);
    
    Object remove(String key);

}
