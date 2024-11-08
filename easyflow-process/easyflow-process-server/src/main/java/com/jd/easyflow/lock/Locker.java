package com.jd.easyflow.lock;

import java.time.Duration;
import java.util.function.Supplier;

/**
 * @author liyuliang5
 *
 */
public interface Locker {

    public String lock(String lockBizType, String bizKey);
    
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond);
    
    public String lock(String lockBizType, String bizKey, boolean throwExceptionOnLock);
    
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond, boolean throwExceptionOnLock);
    
    public String lockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills, boolean throwExceptionOnLock);
    
    public String lockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills, String requestId, boolean throwExceptionOnLock);

    public boolean unlock(String lockBizType, String bizKey, String requestId);
    
    public boolean unlockWithKey(String lockKey, String requestId);
    
    public <T>T doInLock(String lockBizType, String bizKey, Supplier<T> repeatableAction);
    
    public <T>T doInlock(String lockBizType, String bizKey, int lockSecond, int waitSecond, Supplier<T> repeatableAction);
    
    public <T> T doInlockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills, Supplier<T> repeatableAction);
    
}
