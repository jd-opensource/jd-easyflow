package com.jd.easyflow.cache.impl;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.cache.CacheService;

/**
 * @author liyuliang5
 */
public class LocalCacheServiceImpl implements CacheService {
    
    private long setTimeoutMillis = 60 * 1000;
    
    private boolean alwaysGetReturnNull = false;
    
    private Map<String, Value> map = new ConcurrentHashMap<String, Value>();

    @Override
    public String get(String key) {
        if (alwaysGetReturnNull) {
            return null;
        }
        Value value = map.get(key);
        if (value == null) {
            return null;
        }
        if (System.currentTimeMillis() - value.createdMillis <= setTimeoutMillis) {
            return value.value;
        } else {
            map.remove(key);
            return null;
        }
    }
    
    @Override
    public String set(String key, String value) {
        if (alwaysGetReturnNull) {
            return null;
        }
        map.put(key, new Value(value));
        return null;
    }
    
    private static class Value {
        
        public Value(String value) {
            this.value = value;
            this.createdMillis = System.currentTimeMillis();
        }
        private String value;
        
        private long createdMillis;
    }

    public long getSetTimeoutMillis() {
        return setTimeoutMillis;
    }

    public void setSetTimeoutMillis(long setTimeoutMillis) {
        this.setTimeoutMillis = setTimeoutMillis;
    }

    public boolean isAlwaysGetReturnNull() {
        return alwaysGetReturnNull;
    }

    public void setAlwaysGetReturnNull(boolean alwaysGetReturnNull) {
        this.alwaysGetReturnNull = alwaysGetReturnNull;
    }


    
}
