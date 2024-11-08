package com.jd.easyflow.lock.impl;

import java.time.Duration;
import java.util.UUID;
import java.util.function.Supplier;

import com.jd.easyflow.lock.LockService;
import com.jd.easyflow.lock.Locker;

/**
 * 
 */
public class LockerImpl implements Locker {
    
    private LockService lockService;

    @Override
    public String lock(String lockBizType, String bizKey) {
        return lockService.lock(lockBizType, bizKey);
    }

    @Override
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond) {
        return lockService.lock(lockBizType, bizKey, lockSecond, waitSecond);
    }

    @Override
    public String lock(String lockBizType, String bizKey, boolean throwExceptionOnLock) {
        return lockService.lock(lockBizType, bizKey, throwExceptionOnLock);
    }

    @Override
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond,
            boolean throwExceptionOnLock) {
        return lockService.lock(lockBizType, bizKey, lockSecond, waitSecond, throwExceptionOnLock);
    }
    
    
    @Override
    public String lockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills,
            boolean throwExceptionOnLock) {
        return lockService.lockWithKey(lockKey, lockSecond, waitDuration, sleepMills , throwExceptionOnLock);
    }
    
    @Override
    public String lockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills, String requestId,
            boolean throwExceptionOnLock) {
        return lockService.lockWithKey(lockKey, lockSecond, waitDuration, sleepMills, requestId, throwExceptionOnLock);
    }

    @Override
    public boolean unlock(String lockBizType, String bizKey, String requestId) {
        return lockService.unlock(lockBizType, bizKey, requestId);
    }
    
    @Override
    public boolean unlockWithKey(String lockKey, String requestId) {
        return lockService.unlockWithKey(lockKey, requestId);
    }

    @Override
    public <T> T doInLock(String lockBizType, String bizKey, Supplier<T> repeatableAction) {
        return lockService.doInLock(lockBizType, bizKey, repeatableAction);
    }

    @Override
    public <T> T doInlock(String lockBizType, String bizKey, int lockSecond, int waitSecond,
            Supplier<T> repeatableAction) {
        return lockService.doInlock(lockBizType, bizKey, lockSecond, waitSecond, repeatableAction);
    }
    
    @Override
    public <T> T doInlockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills,
            Supplier<T> repeatableAction) {
        return lockService.doInLockWithKey(lockKey, lockSecond, waitDuration, sleepMills,
                UUID.randomUUID().toString() + "|" + System.currentTimeMillis(), true, repeatableAction);
    }

    public LockService getLockService() {
        return lockService;
    }

    public void setLockService(LockService lockService) {
        this.lockService = lockService;
    }
    
}
