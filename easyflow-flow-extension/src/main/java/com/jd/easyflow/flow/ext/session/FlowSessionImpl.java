package com.jd.easyflow.flow.ext.session;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowSessionImpl implements FlowSession {

    private static AtomicLong idStart = new AtomicLong(System.currentTimeMillis());

    private String id;

    private Map<String, Object> data = new ConcurrentHashMap<>();

    public FlowSessionImpl() {
        id = idStart.getAndIncrement() + "";
    }

    @Override
    public void put(String key, Object value) {
        data.put(key, value);
    }

    @Override
    public <T> T get(String key) {
        return (T) data.get(key);
    }

    public Object remove(String key) {
        return data.remove(key);
    }

    public String getId() {
        return id;
    }

}
