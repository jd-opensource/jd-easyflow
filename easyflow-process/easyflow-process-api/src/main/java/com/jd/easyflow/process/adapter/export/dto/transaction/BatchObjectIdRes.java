package com.jd.easyflow.process.adapter.export.dto.transaction;

import java.io.Serializable;
import java.util.Arrays;

/**
 * 
 * @author liyuliang5
 *
 */
public class BatchObjectIdRes implements Serializable {

    String[] ids;

    public String[] getIds() {
        return ids;
    }

    public void setIds(String[] ids) {
        this.ids = ids;
    }

    @Override
    public String toString() {
        return "BatchObjectIdRes [ids=" + Arrays.toString(ids) + "]";
    }
    
    
}
