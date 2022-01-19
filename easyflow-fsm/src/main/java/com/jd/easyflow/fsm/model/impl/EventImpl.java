package com.jd.easyflow.fsm.model.impl;

import com.jd.easyflow.fsm.model.Event;

/**
 * 
 * @author liyuliang5
 *
 */
public class EventImpl implements Event {

    public EventImpl(String id) {
        this.id = id;
    }

    public EventImpl(String id, String name) {
        this.id = id;
        this.name = name;
    }

    private String id;

    private String name;

    @Override
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
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
        EventImpl other = (EventImpl) obj;
        if (id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!id.equals(other.id)) {
            return false;
        }
        return true;
    }

}
