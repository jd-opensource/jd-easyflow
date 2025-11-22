package com.jd.easyflow.objects.factory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.properties.PropertiesAccessor;
import com.jd.easyflow.properties.classpath.ClassPathPropertiesAccessor;

/**
 * @author liyuliang5
 *
 */
public class DefaultObjectFactory implements ObjectFactory {
    
    private static final Logger log = LoggerFactory.getLogger(DefaultObjectFactory.class);

    private static final String KEY_SEPERATOR = " ";

    private String configPath = "/config/objectProvider.properties";

    /**
     * key: objectClassName--providerId--serviceId
     */
    private ConcurrentHashMap<String, Object> cache = new ConcurrentHashMap<>();

    private Map<String, ObjectProvider> providerMap = new ConcurrentHashMap<>();
    
    private PropertiesAccessor propertiesAccessor;

    public void init() {
        if (propertiesAccessor == null) {
            propertiesAccessor = new ClassPathPropertiesAccessor(configPath);
            ((ClassPathPropertiesAccessor) propertiesAccessor).init();
        }
    }

    @Override
    public <T> T getObject(Class<T> clazz) {
        return getObject(clazz, null, null);

    }

    @Override
    public <T> T getObject(Class<T> clazz, String objectIdInProvider) {
        return getObject(clazz, null, objectIdInProvider);
    }

    @Override
    public <T> T getObject(Class<T> clazz, String providerId, String objectIdInProvider) {
        String key = clazz.getName();
        if (providerId != null || objectIdInProvider != null) {
            key += KEY_SEPERATOR + (providerId == null ? "" : providerId) + KEY_SEPERATOR
                    + (objectIdInProvider == null ? "" : objectIdInProvider);
        }
        Object o = cache.get(key);
        if (o != null) {
            return (T) o;
        }
        synchronized (clazz) {
            o = cache.get(clazz);
            if (o != null) {
                return (T) o;
            }
            ObjectProvider provider = null;
            if (providerId != null) {
                provider = providerMap.get(providerId);
                if (provider == null) {
                    throw new EasyFlowException("class:" + clazz.getName() + " providerId:" + providerId + " service provider not found");
                }
            } else {
                String serviceProviderKey = propertiesAccessor
                        .getProperty("provider-" + clazz.getName() + "-" + objectIdInProvider);
                if (serviceProviderKey != null) {
                    provider = providerMap.get(serviceProviderKey);
                    if (provider == null) {
                        throw new EasyFlowException("object provider of " + serviceProviderKey + " not exists");
                    }
                    log.info("Found " + clazz.getName() + " provider type:" + serviceProviderKey + " provider:"
                            + provider.getClass().getName());
                }
                if (providerId == null) {
                    serviceProviderKey = propertiesAccessor.getProperty("provider-" + clazz.getName());
                    if (serviceProviderKey != null) {
                        provider = providerMap.get(serviceProviderKey);
                        if (provider == null) {
                            throw new EasyFlowException("Object provider " + serviceProviderKey + " not exists");
                        }
                        log.info("Found " + clazz.getName() + " provider type:" + serviceProviderKey + " provider:"
                                + provider.getClass().getName());
                    }
                }
                if (provider == null) {
                    String globalProviderKey = propertiesAccessor.getProperty("global-provider");
                    if (globalProviderKey != null) {
                        provider = providerMap.get(globalProviderKey);
                        if (provider == null) {
                            throw new EasyFlowException("Object provider " + globalProviderKey + " not exists");
                        }
                        log.info("Found global provider:" + globalProviderKey + " provider:" + provider.getClass().getName());

                    }
                }
                if (provider == null && providerMap.size() == 1) {
                    provider = providerMap.entrySet().iterator().next().getValue();
                    log.info("Use unique object provider:" + provider.getClass().getName());
                }

                if (provider == null) {
                    throw new EasyFlowException("Not found " + clazz.getName() + " service provider");
                }
            }
            o = provider.getObject(clazz, objectIdInProvider);
            cache.put(key, o);
            return (T) o;
        }
    }

    public String getConfigPath() {
        return configPath;
    }

    public void setConfigPath(String configPath) {
        this.configPath = configPath;
    }

    public Map<String, Object> getCache() {
        return cache;
    }

    public void setCache(ConcurrentHashMap<String, Object> cache) {
        this.cache = cache;
    }

    public Map<String, ObjectProvider> getProviderMap() {
        return providerMap;
    }

    public void setProviderMap(Map<String, ObjectProvider> providerMap) {
        this.providerMap = providerMap;
    }
    



    public PropertiesAccessor getPropertiesAccessor() {
        return propertiesAccessor;
    }

    public void setPropertiesAccessor(PropertiesAccessor propertiesAccessor) {
        this.propertiesAccessor = propertiesAccessor;
    }

}
