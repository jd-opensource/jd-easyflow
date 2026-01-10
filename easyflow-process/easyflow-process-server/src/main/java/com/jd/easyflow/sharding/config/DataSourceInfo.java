package com.jd.easyflow.sharding.config;

import java.util.Properties;

/**
 * @author liyuliang5
 */
public class DataSourceInfo {
    
    private String id;

    private Properties properties;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Properties getProperties() {
        return properties;
    }

    public void setProperties(Properties properties) {
        this.properties = properties;
    }
    
}
