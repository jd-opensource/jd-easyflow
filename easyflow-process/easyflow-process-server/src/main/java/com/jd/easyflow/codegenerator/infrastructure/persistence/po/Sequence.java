package com.jd.easyflow.codegenerator.infrastructure.persistence.po;

import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class Sequence {
    private Long id;

    private String seqKey;

    private String seqSubKey;

    private Long seqValue;

    private Date createdDate;

    private Date modifiedDate;

    private boolean deleted;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getSeqKey() {
        return seqKey;
    }

    public void setSeqKey(String seqKey) {
        this.seqKey = seqKey;
    }

    public String getSeqSubKey() {
        return seqSubKey;
    }

    public void setSeqSubKey(String seqSubKey) {
        this.seqSubKey = seqSubKey;
    }

    public Long getSeqValue() {
        return seqValue;
    }

    public void setSeqValue(Long seqValue) {
        this.seqValue = seqValue;
    }

    public Date getCreatedDate() {
        return createdDate;
    }

    public void setCreatedDate(Date createdDate) {
        this.createdDate = createdDate;
    }

    public Date getModifiedDate() {
        return modifiedDate;
    }

    public void setModifiedDate(Date modifiedDate) {
        this.modifiedDate = modifiedDate;
    }

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName());
        sb.append(" [");
        sb.append("Hash = ").append(hashCode());
        sb.append(", id=").append(id);
        sb.append(", seqKey=").append(seqKey);
        sb.append(", seqSubKey=").append(seqSubKey);
        sb.append(", seqValue=").append(seqValue);
        sb.append(", createdDate=").append(createdDate);
        sb.append(", modifiedDate=").append(modifiedDate);
        sb.append(", deleted=").append(deleted);
        sb.append("]");
        return sb.toString();
    }
}