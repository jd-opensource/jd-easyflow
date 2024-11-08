package com.jd.easyflow.process.client.runtime;

/**
 * @author liyuliang5
 *
 */
public class CacheObject<T> {

    boolean dirty;

    Object id;

    T object;

    int persistOp;

    public boolean isDirty() {
        return dirty;
    }

    public void setDirty(boolean dirty) {
        this.dirty = dirty;
    }

    public Object getId() {
        return id;
    }

    public void setId(Object id) {
        this.id = id;
    }

    public T getObject() {
        return object;
    }

    public void setObject(T object) {
        this.object = object;
    }

    public int getPersistOp() {
        return persistOp;
    }

    public void setPersistOp(int persistOp) {
        this.persistOp = persistOp;
    }

    @Override
    public String toString() {
        return "CacheObject [dirty=" + dirty + ", id=" + id + ", object=" + object + ", persistOp=" + persistOp + "]";
    }
    
    
}
