package com.jd.easyflow.properties.spring;

import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.env.PropertySources;
import org.springframework.core.env.PropertySourcesPropertyResolver;

import com.jd.easyflow.properties.PropertiesAccessor;

/**
 * 
 * @author liyuliang5
 *
 */
public class SpringPropertySourcesPropertiesAccessor implements PropertiesAccessor {

    private PropertySourcesPlaceholderConfigurer configurer;

    private PropertySourcesPropertyResolver resolver;

    public void init() {
        PropertySources sources = configurer.getAppliedPropertySources();
        resolver = new PropertySourcesPropertyResolver(sources);
    }

    @Override
    public String getProperty(String key) {
        return resolver.getProperty(key);
    }

    public PropertySourcesPlaceholderConfigurer getConfigurer() {
        return configurer;
    }

    public void setConfigurer(PropertySourcesPlaceholderConfigurer configurer) {
        this.configurer = configurer;
    }

}
