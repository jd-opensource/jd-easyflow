package com.jd.easyflow.properties.classpath;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.properties.PropertiesAccessor;

/**
 * 
 * @author liyuliang5
 *
 */
public class ClassPathPropertiesAccessor implements PropertiesAccessor {
    
    private static final Logger log = LoggerFactory.getLogger(ClassPathPropertiesAccessor.class);

    
    private String path;
    
    private Properties properties;
    
    public ClassPathPropertiesAccessor() {
        // NOOP
    }
    
    public ClassPathPropertiesAccessor(String path) {
        this.path = path;
    }
    
    public void init() {
        properties = new Properties();
        try {
            InputStream inputStream = this.getClass().getResourceAsStream(path);
            if (inputStream == null) {
                log.warn("Config file " + path +" not exists");
            } else {
                properties.load(inputStream);
            }
        } catch (IOException e) {
            throw new EasyFlowException("Config file " + path + " load exception", e);
        }
    }

    @Override
    public String getProperty(String key) {
        return (String) properties.getProperty(key);
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }
    
    

}
