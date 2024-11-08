package com.jd.easyflow.process.adapter.export.dto.transaction;

import java.io.Serializable;

/**
 * 
 * @author liyuliang5
 *
 */
public class BatchObjectIdReq implements Serializable {

    private String type;

    private int num;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getNum() {
        return num;
    }

    public void setNum(int num) {
        this.num = num;
    }

    @Override
    public String toString() {
        return "BatchObjectIdReq [type=" + type + ", num=" + num + "]";
    }
    
    

}
