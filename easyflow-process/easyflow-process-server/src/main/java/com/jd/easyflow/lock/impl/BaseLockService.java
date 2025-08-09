package com.jd.easyflow.lock.impl;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.lock.LockService;

/**
 * @author liyuliang5
 */
public abstract class BaseLockService implements LockService {
    
    public static final int RENEW_LOCK_SECONDS = -1;

    protected Logger logger = LoggerFactory.getLogger(this.getClass());

    private int defaultLockSeconds = 60 * 60 * 24;

    private int defaultWaitSeconds = 10;
    private int defaultSleepMillis = 50;

    private static final String KEY_PREFIX = "_lock_";
    
    private static final String KEY_SEP = "__";
    
    
    private boolean optimizeByLocalSync = true;
    
    private int retryTimesOnAccquireLockException = 1;
    
    private int retryTimesOnReleaseLockException = 1;
    
    private boolean renewEnabled = false;
    
    private int renewLockSeconds = 60;
    
    private int renewBeforeExpiredSeconds = 40;
    
    private int renewLoopIntervalSeconds = 10;
    
    private Map<String, RenewLockInfo> renewLockMap = new ConcurrentHashMap<String, RenewLockInfo>();
    
        
    public BaseLockService() {
    }
    
    @PostConstruct
    public void init() {
        if (renewEnabled) {
            Thread renewThread = new Thread(() -> {
                loopRenewLocks();
            });
            renewThread.setDaemon(true);
            renewThread.start();
        }
    }

    
    public String lock(String lockBizType, String bizKey) {
        return lock(lockBizType, bizKey, defaultLockSeconds, defaultWaitSeconds, defaultSleepMillis, generateRequestId(), true);
    }

    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond) {
        return lock(lockBizType, bizKey, lockSecond, waitSecond, defaultSleepMillis, generateRequestId(), true);
    }

    public String lock(String lockBizType, String bizKey, boolean throwExceptionOnLock) {
        return lock(lockBizType, bizKey, defaultLockSeconds, defaultWaitSeconds, defaultSleepMillis, generateRequestId(), throwExceptionOnLock);
    }
    
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond, boolean throwExceptionOnLock) {
        return lock(lockBizType, bizKey, lockSecond, waitSecond, defaultSleepMillis, generateRequestId(), throwExceptionOnLock);
    }
    
    public String lock(String lockBizType, String bizKey, int lockSeconds, int waitSeconds, int sleepMills, String requestId,
            boolean throwExceptionOnLock) {
        String key = buildLockKey(lockBizType, bizKey);
        return lockWithKey(key, lockSeconds, Duration.ofSeconds(waitSeconds), sleepMills, requestId, throwExceptionOnLock);
    }
    
    public String lockWithKey(String lockKey, int lockSecond, Duration waitDuration, int sleepMills,
            boolean throwExceptionOnLock) {
        return lockWithKey(lockKey, lockSecond, waitDuration, sleepMills,
                generateRequestId(), throwExceptionOnLock);
    }
    
    public String lockWithKey(String key, int lockSeconds, Duration waitDuration, int sleepMills, String requestId,
                       boolean throwExceptionOnLock) {
        if (lockSeconds < 0) {
            if (lockSeconds == RENEW_LOCK_SECONDS) {
                if (! renewEnabled) {
                    lockSeconds = defaultLockSeconds;
                }
            }
        }
        
        boolean renew = false;
        if (renewEnabled && lockSeconds == RENEW_LOCK_SECONDS) {
            lockSeconds = renewLockSeconds;
            renew = true;
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Start lock, key:" + key);
        }
        if (sleepMills < 5) {
            sleepMills = 5;
        }
        long waitMills = waitDuration.toMillis();
        if (requestId == null) {
            requestId = generateRequestId();
        }
        if (optimizeByLocalSync) {
            addLockThreads(key);
        }
        try {
        while (true) {
            if (!optimizeByLocalSync || isLockAccquireThread(key)) {
                boolean result = accquireLockWithRetry(key, requestId, lockSeconds);
                logger.info("Accquire lock " + result + " key:" + key);
                if (result) {
                    if (renew) {
                        RenewLockInfo lockInfo = new RenewLockInfo(key, requestId, System.currentTimeMillis() + lockSeconds * 1000L, Thread.currentThread().getId(), true);
                        synchronized (renewLockMap) {
                            renewLockMap.put(key, lockInfo);
                        }
                    }
                    break;
                }
            }
            if (waitMills > 0) {
                try {
                    Thread.sleep(sleepMills);
                } catch (InterruptedException e) {
                    throw new EasyFlowException(e);
                }
                waitMills -= sleepMills;
            } else {
                if (throwExceptionOnLock) {
                    throw new EasyFlowException("Lock fail,key:"+ key);
                } else {
                    logger.warn("Lock fail, key:" + key);
                    return null;
                }
            }
        }
        } finally {
            if (optimizeByLocalSync) {
                removeLockThread(key);
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Lock success, requestId:" + requestId);
        }
        return requestId;

    }
    
    protected boolean accquireLockWithRetry(String key, String requestId, int lockSeconds) {
        try {
            boolean result = accquireLock(key, requestId, lockSeconds);
            return result;
        } catch (Exception e) {
            if (retryTimesOnAccquireLockException < 1) {
                throw e;
            }
            logger.warn("accquire lock exception:" + e.getMessage() + ",start retry", e);
            int retryTimes = 0;
            while (true) {
                retryTimes++;
                try {
                    String currentRequestId = queryLock(key);
                    logger.info("retry success, currentRequestId:" + currentRequestId + " requestId:" + requestId);
                    return requestId.equals(currentRequestId);
                } catch (Exception e2) {
                    logger.warn("accquire lock retry exception:" + e.getMessage(), e);
                    if (retryTimes >= retryTimesOnAccquireLockException) {
                        throw e2;
                    }
                }
            }
        }

    }
    
    protected abstract boolean accquireLock(String key, String requestId, int lockSeconds);
    
    protected boolean releaseLockWithRetry(String key, String requestId) {
        try {
            boolean result = releaseLock(key, requestId);
            return result;
        } catch (Exception e) {
            if (retryTimesOnReleaseLockException < 1) {
                throw e;
            }
            logger.warn("release lock exception:" + e.getMessage() + ", start retry", e);
            int retryTimes = 0;
            while (true) {
                retryTimes++;
                try {
                    boolean result = releaseLock(key, requestId);
                    logger.info("retry success, result:" + result);
                    return result;
                } catch (Exception e2) {
                    logger.warn("release lock retry exception," + e.getMessage(), e);
                    if (retryTimes >= retryTimesOnReleaseLockException) {
                        throw e2;
                    }
                }

            }
        }
    }
    
    protected abstract boolean releaseLock(String key, String requestId);
    
    protected abstract String queryLock(String key);
    
    protected abstract boolean renewLock(String key, String requestId, int renewSeconds);
    
    
    private volatile Map<String, List<Long>> localLockMap = new ConcurrentHashMap<String, List<Long>>();
    
    private void addLockThreads(String lockKey) {
        long threadId = Thread.currentThread().getId();
        localLockMap.compute(lockKey, (key, value) -> {
            if (value == null) {
                value = Collections.synchronizedList(new ArrayList<Long>(1));
            }
            value.add(threadId);
            return value;
        });
    }
    
    private boolean isLockAccquireThread(String lockKey) {
        List<Long> lockThreadList = localLockMap.get(lockKey);
        return lockThreadList.get(0) == Thread.currentThread().getId();
    }

    private void removeLockThread(String lockKey) {
        long threadId = Thread.currentThread().getId();
        localLockMap.compute(lockKey, (key, value) -> {
            value.remove(threadId);
            return value.size() == 0 ? null : value;
        });
    }
    
    protected String generateRequestId() {
        return UUID.randomUUID().toString() + "|" + System.currentTimeMillis();
    }

    public boolean unlock(String lockBizType, String bizKey, String requestId) {
        String key = buildLockKey(lockBizType, bizKey);
        return unlockWithKey(key, requestId);
    }

    public boolean unlockWithKey(String key, String requestId) {
        if (requestId == null) {
            logger.info("lock key:" + key + "requestId is null, return");
            return false;
        }
        logger.info("Unlock:key:" + key + " requestId:" + requestId);
        if (renewEnabled) {
            synchronized (renewLockMap) {
                renewLockMap.computeIfPresent(key, (k, v) -> {
                    v.valid = false;
                    return null;
                });
            }
        }
        boolean result = releaseLockWithRetry(key, requestId);
        if (! result) {
            logger.warn("Unlock fail, may unlocked or lock not exists, key:" + key, " requestId" + requestId);
            return false;
        }
        return true;
    }
    
    public <T>T doInLock(String lockBizType, String bizKey, Supplier<T> repeatableAction) {
        return doInLock(lockBizType, bizKey, defaultLockSeconds, defaultWaitSeconds, defaultSleepMillis, generateRequestId(), true, repeatableAction);
    }
    
    public <T>T doInlock(String lockBizType, String bizKey, int lockSecond, int waitSecond, Supplier<T> repeatableAction) {
        return doInLock(lockBizType, bizKey, lockSecond, waitSecond, defaultSleepMillis, generateRequestId(), true, repeatableAction);
    }
    
    private <T> T doInLock(String lockBizType, String bizKey, int lockSeconds, int waitSeconds, int sleepMills,
            String requestId, boolean throwExceptionOnLock, Supplier<T> repeatableAction) {
        requestId = lock(lockBizType, bizKey, lockSeconds, waitSeconds, sleepMills, requestId, throwExceptionOnLock);
        if (requestId == null) {
            return null;
        }
        try {
            return repeatableAction.get();
        } finally {
            unlock(lockBizType, bizKey, requestId);
        }
    }
    
    
    public <T> T doInLockWithKey(String lockKey, int lockSeconds, Duration waitDuration, int sleepMills,
            String requestId, boolean throwExceptionOnLock, Supplier<T> repeatableAction) {
        requestId = lockWithKey(lockKey, lockSeconds, waitDuration, sleepMills, requestId, throwExceptionOnLock);
        if (requestId == null) {
            return null;
        }
        try {
            return repeatableAction.get();
        } finally {
            unlockWithKey(lockKey, requestId);
        }
    }
    
    private void loopRenewLocks() {
        logger.info("start loop renew locks");
        while (true) {
            try {
                renewLocks();
                Thread.sleep(renewLoopIntervalSeconds * 1000);
            } catch (Throwable t) {
                logger.error("loop renew locks exception " + t.getMessage(), t);
            }
        }
    }

    private void renewLocks() {
        List<RenewLockInfo> renewLockInfoList = new ArrayList<RenewLockInfo>(renewLockMap.size());
        synchronized(renewLockMap) {
            renewLockInfoList.addAll(renewLockMap.values());
        }
        boolean loggerDebug = logger.isDebugEnabled();
        if (loggerDebug) {
            logger.debug("renew lock size:" + renewLockInfoList.size());
        }
        for (RenewLockInfo lockInfo : renewLockInfoList) {
            if (loggerDebug) {
                logger.debug("renew lock:" + lockInfo);
            }
            if (!lockInfo.valid) {
                continue;
            }
            long currentTime = System.currentTimeMillis();
            if (lockInfo.expireTime - renewBeforeExpiredSeconds * 1000L < currentTime) {
                try {
                    renewLock(lockInfo.key, lockInfo.requestId, renewLockSeconds);
                    lockInfo.renewTimes++;
                } catch (Exception e) {
                    logger.warn("renewlock exception, key:" +lockInfo.key + "," + e.getMessage(), e);
                }
            }
        }

    }
    
    private String buildLockKey(String lockBizType, String bizKey) {
        return KEY_PREFIX + lockBizType + KEY_SEP + bizKey;
    }

    public boolean isOptimizeByLocalSync() {
        return optimizeByLocalSync;
    }

    public void setOptimizeByLocalSync(boolean optimizeByLocalSync) {
        this.optimizeByLocalSync = optimizeByLocalSync;
    }

    
    public int getRetryTimesOnAccquireLockException() {
        return retryTimesOnAccquireLockException;
    }

    public void setRetryTimesOnAccquireLockException(int retryTimesOnAccquireLockException) {
        this.retryTimesOnAccquireLockException = retryTimesOnAccquireLockException;
    }

    public int getRetryTimesOnReleaseLockException() {
        return retryTimesOnReleaseLockException;
    }

    public void setRetryTimesOnReleaseLockException(int retryTimesOnReleaseLockException) {
        this.retryTimesOnReleaseLockException = retryTimesOnReleaseLockException;
    }

    public boolean isRenewEnabled() {
        return renewEnabled;
    }


    public void setRenewEnabled(boolean renewEnabled) {
        this.renewEnabled = renewEnabled;
    }


    public int getRenewLockSeconds() {
        return renewLockSeconds;
    }


    public void setRenewLockSeconds(int renewLockSeconds) {
        this.renewLockSeconds = renewLockSeconds;
    }


    public int getRenewBeforeExpiredSeconds() {
        return renewBeforeExpiredSeconds;
    }


    public void setRenewBeforeExpiredSeconds(int renewBeforeExpiredSeconds) {
        this.renewBeforeExpiredSeconds = renewBeforeExpiredSeconds;
    }


    public int getRenewLoopIntervalSeconds() {
        return renewLoopIntervalSeconds;
    }


    public void setRenewLoopIntervalSeconds(int renewLoopIntervalSeconds) {
        this.renewLoopIntervalSeconds = renewLoopIntervalSeconds;
    }


    public Map<String, RenewLockInfo> getRenewLockMap() {
        return renewLockMap;
    }


    public void setRenewLockMap(Map<String, RenewLockInfo> renewLockMap) {
        this.renewLockMap = renewLockMap;
    }
    

    public int getDefaultLockSeconds() {
        return defaultLockSeconds;
    }


    public void setDefaultLockSeconds(int defaultLockSeconds) {
        this.defaultLockSeconds = defaultLockSeconds;
    }


    public int getDefaultWaitSeconds() {
        return defaultWaitSeconds;
    }


    public void setDefaultWaitSeconds(int defaultWaitSeconds) {
        this.defaultWaitSeconds = defaultWaitSeconds;
    }


    public int getDefaultSleepMillis() {
        return defaultSleepMillis;
    }


    public void setDefaultSleepMillis(int defaultSleepMillis) {
        this.defaultSleepMillis = defaultSleepMillis;
    }





    private static class RenewLockInfo {

        private String key;
        
        private String requestId;
        
        private long expireTime;
        
        private long lockThreadId;
        
        private boolean valid;
        
        private int renewTimes;
        
        public RenewLockInfo() {
            
        }
        
        
        public RenewLockInfo(String key, String requestId, long expireTime, long lockThreadId, boolean valid) {
            this.key = key;
            this.requestId = requestId;
            this.expireTime = expireTime;
            this.lockThreadId = lockThreadId;
            this.valid = valid;
        }


        @Override
        public String toString() {
            return "RenewLockInfo [key=" + key + ", requestId=" + requestId + ", expireTime=" + expireTime
                    + ", lockThreadId=" + lockThreadId + ", valid=" + valid + ", renewTimes=" + renewTimes + "]";
        }
        
    }


}
