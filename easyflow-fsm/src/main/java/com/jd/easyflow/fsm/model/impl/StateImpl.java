package com.jd.easyflow.fsm.model.impl;

import java.util.Map;

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
        this.properties = properties;
    }

    private String id;

    private String name;

    private Map<String, Object> properties;

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
        this.properties = properties;
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
        if (properties == null) {
            return null;
        }
        return (T) properties.get(key);
    }

}
