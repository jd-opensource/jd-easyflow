package com.jd.easyflow.action;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 
 * @author liyuliang5
 *
 */
public class ActionInfo<P, R> {


    private P param;


    private R result;
    

    private String actionCode;


    private Map<String, Object> contextData = new ConcurrentHashMap<>();

    public P getParam() {
        return param;
    }

    public void setParam(P param) {
        this.param = param;
    }

    public R getResult() {
        return result;
    }

    public void setResult(R result) {
        this.result = result;
    }

    public void put(String key, Object value) {
        contextData.put(key, value);
    }

    public <T> T get(String key) {
        return (T) contextData.get(key);
    }

    public String getActionCode() {
        return actionCode;
    }

    public void setActionCode(String actionCode) {
        this.actionCode = actionCode;
    }
    
    

}
