package com.jd.easyflow.cache.impl;

import com.jd.easyflow.cache.CacheService;

/**
 * @author liyuliang5
 *
 */
public class UnsupportedCacheServiceImpl implements CacheService {


    @Override
    public String set(String key, String value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String get(String key) {
        throw new UnsupportedOperationException();
        
    }


}
