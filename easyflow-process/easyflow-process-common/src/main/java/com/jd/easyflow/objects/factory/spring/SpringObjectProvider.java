package com.jd.easyflow.objects.factory.spring;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.objects.factory.ObjectProvider;
import com.jd.easyflow.properties.PropertiesAccessor;
import com.jd.easyflow.properties.classpath.ClassPathPropertiesAccessor;

/**
 * @author liyuliang5
 *
 */
public class SpringObjectProvider implements ObjectProvider, ApplicationContextAware {
    
    private static final Logger log = LoggerFactory.getLogger(SpringObjectProvider.class);


    private String configPath = "/config/objectProvider.properties";

    private ApplicationContext context;

    private static final String BEAN_ID_PREFIX = "spring.bean.id.";

    private PropertiesAccessor propertiesAccessor;

    public void init() {
        if (propertiesAccessor == null) {
            propertiesAccessor = new ClassPathPropertiesAccessor(configPath);
            ((ClassPathPropertiesAccessor) propertiesAccessor).init();
        }
    }

    @Override
    public <T> T getObject(Class<T> clazz, String serviceId) {
        if (serviceId == null) {
            serviceId = propertiesAccessor.getProperty(BEAN_ID_PREFIX + clazz.getName());
        }
        if (serviceId == null) {
            return context.getBean(clazz);
        } else {
            Object result = context.getBean(serviceId);
            if (result != null && clazz != null) {
                if (!(clazz.isAssignableFrom(result.getClass()))) {
                    throw new EasyFlowException(
                            "bean:" + serviceId + " real type:" + result.getClass().getName() + " expected type:" + clazz.getName());
                }
            }
            return (T) result;
        }
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.context = applicationContext;
    }

    public String getConfigPath() {
        return configPath;
    }

    public void setConfigPath(String configPath) {
        this.configPath = configPath;
    }

    public PropertiesAccessor getPropertiesAccessor() {
        return propertiesAccessor;
    }

    public void setPropertiesAccessor(PropertiesAccessor propertiesAccessor) {
        this.propertiesAccessor = propertiesAccessor;
    }

}
