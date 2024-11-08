package com.jd.easyflow.objects.factory;

/**
 * @author liyuliang5
 *
 */
public class ObjectFactorys {

    private static ObjectFactory defaultObjectFactory;

    public static ObjectFactory getDefault() {
        return defaultObjectFactory;
    }
    
    public void setDefaultObjectFactory(ObjectFactory objectFactory) {
        ObjectFactorys.defaultObjectFactory = objectFactory;
    }
    
}
