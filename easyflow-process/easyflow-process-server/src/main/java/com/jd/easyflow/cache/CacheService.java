package com.jd.easyflow.cache;

/**
 * @author liyuliang5
 *
 */
public interface CacheService {

    String set(final String key, final String value);
    
    String get(final String key);
    
}
