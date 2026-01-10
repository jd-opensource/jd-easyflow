package com.jd.easyflow.processunit.domain.model.vo;

import java.util.Date;
import java.util.List;

/**
 * 
 * @author liyuliang5
 */
public class ShardingMigrateContext {
    
    private List<String> unitCodeList;
    private Date createdDateStart = null;
    private Date createdDateEnd = null;
    private int pageSize = 1000;
    private int threadCount = 1;
    private boolean lock;
    private boolean migrateExecution;
    private String instanceNo;
    private List<String> resultList;
    private boolean newId;
    
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
    public boolean isMigrateExecution() {
        return migrateExecution;
    }
    public void setMigrateExecution(boolean migrateExecution) {
        this.migrateExecution = migrateExecution;
    }
    public String getInstanceNo() {
        return instanceNo;
    }
    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }
    public boolean isNewId() {
        return newId;
    }
    public void setNewId(boolean newId) {
        this.newId = newId;
    }
    public List<String> getResultList() {
        return resultList;
    }
    public void setResultList(List<String> resultList) {
        this.resultList = resultList;
    }
    
    
    
    

}
