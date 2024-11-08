package com.jd.easyflow.lock.impl;

import java.time.Duration;
import java.util.function.Supplier;

import com.jd.easyflow.lock.Locker;

/**
 * @author liyuliang5
 */
public class NoopLocker implements Locker {
    

    @Override
    public String lock(String lockBizType, String bizKey) {
        return null;
    }

    @Override
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond) {
        return null;
    }

    @Override
    public String lock(String lockBizType, String bizKey, boolean throwExceptionOnLock) {
        return null;
    }

    @Override
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond,
            boolean throwExceptionOnLock) {
        return null;
    }

    @Override
    public String lockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills,
            boolean throwExceptionOnLock) {
        return null;
    }

    @Override
    public String lockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills, String requestId,
            boolean throwExceptionOnLock) {
        return null;
    }

    @Override
    public boolean unlock(String lockBizType, String bizKey, String requestId) {
        return true;
    }

    @Override
    public boolean unlockWithKey(String lockKey, String requestId) {
        return true;
    }

    @Override
    public <T> T doInLock(String lockBizType, String bizKey, Supplier<T> repeatableAction) {
        return repeatableAction.get();
    }

    @Override
    public <T> T doInlock(String lockBizType, String bizKey, int lockSecond, int waitSecond,
            Supplier<T> repeatableAction) {
        return repeatableAction.get();
    }

    @Override
    public <T> T doInlockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills,
            Supplier<T> repeatableAction) {
        return repeatableAction.get();
    }

}
