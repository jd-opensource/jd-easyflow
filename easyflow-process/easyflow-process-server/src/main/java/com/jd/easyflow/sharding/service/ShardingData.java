package com.jd.easyflow.sharding.service;

/**
 * @author liyuliang5
 */
public class ShardingData {
    
    private String shard;
    
    private Boolean usingSlave;
    
    private String group;
    
    private String bizNo;

    public String getShard() {
        return shard;
    }

    public void setShard(String shard) {
        this.shard = shard;
    }


    public Boolean getUsingSlave() {
        return usingSlave;
    }

    public void setUsingSlave(Boolean usingSlave) {
        this.usingSlave = usingSlave;
    }

    public String getGroup() {
        return group;
    }

    public void setGroup(String group) {
        this.group = group;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }
    
    

}
