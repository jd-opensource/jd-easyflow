package com.jd.easyflow.objects.factory;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ObjectFactory {
    
    /**
     * Find object by class with default provider.
     * @param <T>
     * @param clazz
     * @return
     */
    <T> T getObject(Class<T> clazz);
    
    /**
     * Find object by class and object id.
     * @param <T>
     * @param clazz
     * @param serviceIdInProvider
     * @return
     */
    <T> T getObject(Class<T> clazz, String objectIdInProvider);
    
    /**
     * Find object by class,providerId and objectIdInProvider.
     * @param <T>
     * @param clazz
     * @param provider
     * @param objectIdInProvider
     * @return
     */
    <T> T getObject(Class<T> clazz, String providerId, String objectIdInProvider);
    
}
