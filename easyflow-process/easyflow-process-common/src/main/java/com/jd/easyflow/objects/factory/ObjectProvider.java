package com.jd.easyflow.objects.factory;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ObjectProvider {
    
    <T>T getObject(Class<T> clazz, String id);
}
