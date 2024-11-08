package com.jd.easyflow.lock;

import java.util.function.Supplier;

import com.jd.easyflow.objects.factory.ObjectFactory;
import com.jd.easyflow.objects.factory.ObjectFactorys;

/**
 * @author liyuliang5
 *
 */
public class LockUtil {

    public static String lock(String lockBizType, String bizKey) {
        return ObjectFactorys.getDefault().getObject(Locker.class).lock(lockBizType, bizKey);
    }
    
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond) {
        return ObjectFactorys.getDefault().getObject(Locker.class).lock(lockBizType, bizKey, lockSecond, waitSecond);
    }
    
    public String lock(String lockBizType, String bizKey, boolean throwExceptionOnLock) {
        return ObjectFactorys.getDefault().getObject(Locker.class).lock(lockBizType, bizKey, throwExceptionOnLock);
    }
    
    public String lock(String lockBizType, String bizKey, int lockSecond, int waitSecond, boolean throwExceptionOnLock) {
        return ObjectFactorys.getDefault().getObject(Locker.class).lock(lockBizType, bizKey, lockSecond, waitSecond, throwExceptionOnLock);
    }
    
    public boolean unlock(String lockBizType, String bizKey, String requestId) {
        return ObjectFactorys.getDefault().getObject(Locker.class).unlock(lockBizType, bizKey, requestId);
    }
    
    public <T>T doInLock(String lockBizType, String bizKey, Supplier<T> repeatableAction) {
        return ObjectFactorys.getDefault().getObject(Locker.class).doInLock(lockBizType, bizKey, repeatableAction);
    }
    
    public <T>T doInlock(String lockBizType, String bizKey, int lockSecond, int waitSecond, Supplier<T> repeatableAction) {
        return ObjectFactorys.getDefault().getObject(Locker.class).doInlock(lockBizType, bizKey, lockSecond, waitSecond, repeatableAction);
    }
}
