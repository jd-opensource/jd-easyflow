package com.jd.easyflow.cache.impl;

import java.util.Map;
import java.util.Set;

import com.jd.easyflow.cache.CacheService;

/**
 * No Operation Cache Service
 * @author liyuliang5
 *
 */
public class NoopCacheServiceImpl implements CacheService {

    @Override
    public Map<String, String> hgetAll(String key) {
        // NOOP
        return null;
    }

    @Override
    public void del(String key) {
        // NOOP
        
    }

    @Override
    public Long hset(String key, String field, String value) {
        // NOOP
        return null;
    }

    @Override
    public String hmset(String key, Map<String, String> hash) {
        // NOOP
        return null;
    }

    @Override
    public Long expire(String key, int seconds) {
        // NOOP
        return null;
    }

    @Override
    public String set(String key, String value) {
        // NOOP
        return null;
    }
    
    @Override
    public Long setnx(String key, String value) {
        // NOOP
        return null;
    }

    @Override
    public String get(String key) {
        // NOOP
        return null;
    }

    @Override
    public String hget(String key, String field) {
        // NOOP
        return null;
    }

    @Override
    public Set<String> smembers(String key) {
        // NOOP
        return null;
    }

    @Override
    public Long sadd(String key, String... members) {
        // NOOP
        return null;
    }

    @Override
    public Long hdel(String key, String... field) {
        // NOOP
        return null;
    }

    @Override
    public Long srem(String key, String... members) {
        // NOOP
        return null;
    }

    @Override
    public Boolean exists(String key) {
        // NOOP
        return null;
    }

    @Override
    public Set<String> hkeys(String key) {
        // NOOP
        return null;
    }

    @Override
    public Long incrBy(String key, long delta) {
        // NOOP
        return null;
    }

    @Override
    public Long hincrBy(String key, String hashKey, long delta) {
        // NOOP
        return null;
    }

    @Override
    public Double hincrBy(String key, String hashKey, double value) {
        // NOOP
        return null;
    }

}
