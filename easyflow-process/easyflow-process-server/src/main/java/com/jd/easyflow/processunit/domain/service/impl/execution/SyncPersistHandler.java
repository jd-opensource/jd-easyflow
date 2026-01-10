package com.jd.easyflow.processunit.domain.service.impl.execution;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.processunit.domain.service.ProcessUnitExecutionPersistHandler;

/**
 * @author: chenzhenghao1
 */
public class SyncPersistHandler implements ProcessUnitExecutionPersistHandler {
    
    private static final Logger log = LoggerFactory.getLogger(SyncPersistHandler.class);


    @Autowired
    private ProcessUnitRepository processUnitRepository;

    @Override
    public void persistBeforeCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        if (log.isDebugEnabled()) {
            log.debug("Sync save execution instance:" + processUnitExecution);
        }
        processUnitRepository.saveExecution(processUnitExecution);
    }

    @Override
    public void persistAfterCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        if (log.isDebugEnabled()) {
            log.debug("Sync update execution instance:" + processUnitExecution);
        }
        processUnitRepository.updateExecutionByExecutionNo(processUnitExecution);
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }
    
    
}
