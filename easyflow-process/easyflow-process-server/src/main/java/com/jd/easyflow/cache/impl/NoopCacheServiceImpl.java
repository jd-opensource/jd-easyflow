package com.jd.easyflow.cache.impl;

import com.jd.easyflow.cache.CacheService;

/**
 * No Operation Cache Service
 * @author liyuliang5
 *
 */
public class NoopCacheServiceImpl implements CacheService {


    @Override
    public String set(String key, String value) {
        // NOOP
        return null;
    }

    @Override
    public String get(String key) {
        // NOOP
        return null;
    }
    

}
