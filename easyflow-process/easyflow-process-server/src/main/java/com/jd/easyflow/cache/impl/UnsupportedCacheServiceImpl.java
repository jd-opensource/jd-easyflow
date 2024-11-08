package com.jd.easyflow.cache.impl;

import java.util.Map;
import java.util.Set;

import com.jd.easyflow.cache.CacheService;

/**
 * @author liyuliang5
 *
 */
public class UnsupportedCacheServiceImpl implements CacheService {

    @Override
    public Map<String, String> hgetAll(String key) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public void del(String key) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Long hset(String key, String field, String value) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public String hmset(String key, Map<String, String> hash) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Long expire(String key, int seconds) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public String set(String key, String value) {
        throw new UnsupportedOperationException();
    }
    
    @Override
    public Long setnx(String key, String value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String get(String key) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public String hget(String key, String field) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Set<String> smembers(String key) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Long sadd(String key, String... members) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Long hdel(String key, String... field) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Long srem(String key, String... members) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Boolean exists(String key) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Set<String> hkeys(String key) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Long incrBy(String key, long delta) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Long hincrBy(String key, String hashKey, long delta) {
        throw new UnsupportedOperationException();
        
    }

    @Override
    public Double hincrBy(String key, String hashKey, double value) {
        throw new UnsupportedOperationException();
    }

}
