package com.jd.easyflow.fsm.model.impl;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.fsm.model.State;

/**
 * 
 * @author liyuliang5
 *
 */
public class StateImpl implements State {

    public StateImpl(String id) {
        this.id = id;
    }

    public StateImpl(String id, String name) {
        this.id = id;
        this.name = name;
    }

    public StateImpl(String id, String name, Map<String, Object> properties) {
        this.id = id;
        this.name = name;
        setProperties(properties);
    }

    private String id;

    private String name;

    private Map<String, Object> properties = new ConcurrentHashMap<String, Object>();

    @Override
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties.clear();
        putProperties(properties);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((id == null) ? 0 : id.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        StateImpl other = (StateImpl) obj;
        if (id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!id.equals(other.id)) {
            return false;
        }
        return true;
    }
    
    @Override
    public <T> T getProperty(String key) {
        return (T) properties.get(key);
    }

    @Override
    public void setProperty(String key, Object value) {
        if (value == null) {
            properties.remove(key);
        } else {
            properties.put(key, value);
        }
    }
    
    public void putProperties(Map<String, Object> properties) {
        if (properties == null) {
            return;
        }
        for (Entry<String, Object> entry : properties.entrySet()) {
            if (entry.getValue() == null) {
                this.properties.remove(entry.getKey());
            } else {
                this.properties.put(entry.getKey(), entry.getValue());
            }
        }
    }

}
