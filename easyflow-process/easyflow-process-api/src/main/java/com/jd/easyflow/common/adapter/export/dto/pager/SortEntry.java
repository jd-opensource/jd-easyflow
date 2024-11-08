package com.jd.easyflow.common.adapter.export.dto.pager;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class SortEntry implements Comparable<SortEntry>, Serializable {

    private static final long serialVersionUID = 9034251346323773481L;

    /**
     * ORDER ASC
     */
    public static final String ASC = "asc";

    /**
     * ORDER DESC
     */
    public static final String DESC = "desc";

    /**
     * Input key
     */
    private String key;

    /**
     * Input type
     */
    private String type = ASC;

    private long seq;
    
    public SortEntry() {
        
    }

    public SortEntry(String key, String type) {
        this.key = key;
        this.type = type;
    }

    public SortEntry(String key, long seq, String type) {
        this.key = key;
        this.type = type;
        this.seq = seq;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public long getSeq() {
        return seq;
    }

    public void setSeq(long seq) {
        this.seq = seq;
    }

    @Override
    public int compareTo(SortEntry o) {
        if (o.getSeq() == this.getSeq()) {
            return 0;
        }
        if (this.getSeq() > o.getSeq()) {
            return 1;
        }
        return -1;
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public String toString() {
        return "SortEntry [key=" + key + ", type=" + type + ", seq=" + seq + "]";
    }
    
    

}
