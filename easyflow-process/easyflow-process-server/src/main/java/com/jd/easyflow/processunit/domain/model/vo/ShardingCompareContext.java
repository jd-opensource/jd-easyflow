package com.jd.easyflow.processunit.domain.model.vo;

import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

/**
 * 
 * @author liyuliang5
 */
public class ShardingCompareContext {

    private List<String> unitCodeList;
    private Date createdDateStart = null;
    private Date createdDateEnd = null;
    private int pageSize = 1000;
    private int threadCount = 1;
    private boolean lock;
    private AtomicLong failCount = new AtomicLong();
    private List<String> resultList;
    private String instanceNo;
    public List<String> getUnitCodeList() {
        return unitCodeList;
    }
    public void setUnitCodeList(List<String> unitCodeList) {
        this.unitCodeList = unitCodeList;
    }
    public Date getCreatedDateStart() {
        return createdDateStart;
    }
    public void setCreatedDateStart(Date createdDateStart) {
        this.createdDateStart = createdDateStart;
    }
    public Date getCreatedDateEnd() {
        return createdDateEnd;
    }
    public void setCreatedDateEnd(Date createdDateEnd) {
        this.createdDateEnd = createdDateEnd;
    }
    public int getPageSize() {
        return pageSize;
    }
    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }
    public int getThreadCount() {
        return threadCount;
    }
    public void setThreadCount(int threadCount) {
        this.threadCount = threadCount;
    }
    public boolean isLock() {
        return lock;
    }
    public void setLock(boolean lock) {
        this.lock = lock;
    }
    public AtomicLong getFailCount() {
        return failCount;
    }
    public void setFailCount(AtomicLong failCount) {
        this.failCount = failCount;
    }
    public String getInstanceNo() {
        return instanceNo;
    }
    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }
    public List<String> getResultList() {
        return resultList;
    }
    public void setResultList(List<String> resultList) {
        this.resultList = resultList;
    }
    
    
    
}
