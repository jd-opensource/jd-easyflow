package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class PersistDTO implements Serializable {

    private int persistOp;
    
    private Object persistObject;

    public int getPersistOp() {
        return persistOp;
    }

    public void setPersistOp(int persistOp) {
        this.persistOp = persistOp;
    }

    public Object getPersistObject() {
        return persistObject;
    }

    public void setPersistObject(Object persistObject) {
        this.persistObject = persistObject;
    }

    @Override
    public String toString() {
        return "PersistDTO [persistOp=" + persistOp + ", persistObject=" + persistObject + "]";
    }
    
    
}
