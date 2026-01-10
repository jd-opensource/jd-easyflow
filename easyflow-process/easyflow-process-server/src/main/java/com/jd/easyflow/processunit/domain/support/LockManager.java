package com.jd.easyflow.processunit.domain.support;

import java.util.function.Supplier;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.lock.Locker;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;

/**
 * @author liyuliang5
 */
public class LockManager {
    
    @Autowired
    private ProcessUnitRepository processUnitRepository;
    
    private Locker locker;
    
    public <T> T doInlock(String unitCode, String bizNo, String scene, Supplier<T> repeatableAction) {
        ProcessUnitEntity unit = processUnitRepository.getProcessUnitByCode(unitCode);
        Integer lockSeconds = (Integer) unit.getConfig(ProcessUnitConstants.CONF_LOCK_SECONDS);
        if (lockSeconds == null) {
            lockSeconds = ProcessUnitConstants.DEFAULT_LOCK_SECONDS;
        }
        return locker.doInlock(ProcessUnitConstants.LOCK_BIZ_TYPE, unitCode + ProcessUnitConstants.LOCK_KEY_SEP + bizNo,
                lockSeconds, ProcessUnitConstants.DEFAULT_WAIT_SECONDS, () -> {
                    return repeatableAction.get();
                });
    }
    
    public String lock(String unitCode, String bizNo, String scene) {
        ProcessUnitEntity unit = processUnitRepository.getProcessUnitByCode(unitCode);
        Integer lockSeconds = (Integer) unit.getConfig(ProcessUnitConstants.CONF_LOCK_SECONDS);
        if (lockSeconds == null) {
            lockSeconds = ProcessUnitConstants.DEFAULT_LOCK_SECONDS;
        }
        return locker.lock(ProcessUnitConstants.LOCK_BIZ_TYPE,
                unitCode + ProcessUnitConstants.LOCK_KEY_SEP + bizNo, lockSeconds,
                ProcessUnitConstants.DEFAULT_WAIT_SECONDS);
    }



    public Locker getLocker() {
        return locker;
    }

    public void setLocker(Locker locker) {
        this.locker = locker;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }
    
    public boolean unlock(String unitCode, String bizNo, String scene, String requestId) {
        return locker.unlock(ProcessUnitConstants.LOCK_BIZ_TYPE, unitCode
                + ProcessUnitConstants.LOCK_KEY_SEP + bizNo, requestId);
    }

    
}
