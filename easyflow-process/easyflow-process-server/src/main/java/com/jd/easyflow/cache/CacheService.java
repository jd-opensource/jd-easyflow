package com.jd.easyflow.cache;

import java.util.Map;
import java.util.Set;

/**
 * @author liyuliang5
 *
 */
public interface CacheService {

    Map<String, String> hgetAll(String key);
    
    void del(String key);

    Long hset(final String key,final String field,final String value);
    
    String hmset(final String key, final Map<String, String> hash);
    
    Long expire(final String key, final int seconds);

    String set(final String key, final String value);
    
    Long setnx(final String key, final String value);

    String get(final String key);

    String hget(String key, String field);

    Set<String> smembers(String key);

    Long sadd(final String key, final String... members);
    
    Long hdel(final String key, final String... field);
    
    Long srem(final String key, final String... members);

    Boolean exists(final String key);
    
    Set<String> hkeys(String key);
    
    Long incrBy(String key, long delta);
    
    Long hincrBy(String key, String hashKey, long delta);
    
    Double hincrBy(String key, String hashKey, double value);
}
