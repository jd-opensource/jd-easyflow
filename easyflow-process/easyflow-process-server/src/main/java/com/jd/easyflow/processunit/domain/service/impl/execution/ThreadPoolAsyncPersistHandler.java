package com.jd.easyflow.processunit.domain.service.impl.execution;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

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

public class ThreadPoolAsyncPersistHandler implements ProcessUnitExecutionPersistHandler {
    
    private static final Logger log = LoggerFactory.getLogger(ThreadPoolAsyncPersistHandler.class);


    @Autowired
    private ProcessUnitRepository processUnitRepository;

    private Executor threadPoolExecutor;


    @Override
    public void persistBeforeCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        if (log.isDebugEnabled()) {
            log.debug("Async persist before call:" + processUnitExecution);
        }
        String version = context.getParam().getVersion();
        if (null == version) {
            if (log.isDebugEnabled()) {
                log.debug("Save execution instance");
            }
            processUnitRepository.saveExecution(processUnitExecution);
        } else {
            CompletableFuture.runAsync(() -> {
                if (log.isDebugEnabled()) {
                    log.debug("Save execution instance");
                }
                try {
                    processUnitRepository.saveExecution(processUnitExecution);
                } catch (Exception e) {
                    log.error("Save execution instance exception," + processUnitExecution + " " + e.getMessage(), e);
                }
            }, threadPoolExecutor);
        }
    }

    @Override
    public void persistAfterCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        if (log.isDebugEnabled()) {
            log.debug("Async persist after call:" + processUnitExecution);
        }
        CompletableFuture.runAsync(() -> {
            try {
                updateOrInsert(processUnitExecution);
            } catch (Exception e) {
                log.error("Execution instance update exception," + processUnitExecution + " " + e.getMessage(), e);
            }
        }, threadPoolExecutor);
    }

    private void updateOrInsert(ProcessUnitExecutionEntity processUnitExecution) {
        if (log.isDebugEnabled()) {
            log.debug("Update or insert execution instance:" + processUnitExecution);
        }
        ProcessUnitExecutionEntity execution = null;
        int count = 5;
        while (Objects.isNull(execution) && count > 0) {
            execution = processUnitRepository.getExecution(processUnitExecution.getExecutionNo(), processUnitExecution.getProcessUnitCode(), processUnitExecution.getBizNo());
            try {
                TimeUnit.MILLISECONDS.sleep(200);
            } catch (InterruptedException e) {
                log.info("Async persist thread {} interrupted", Thread.currentThread().getName(), e);
            }
            count--;
        }
        if (Objects.isNull(execution)) {
            log.info("Processunit execution {} spin timeout, insert record{}", processUnitExecution.getExecutionNo(), processUnitExecution);
            processUnitRepository.saveExecution(processUnitExecution);
        } else {
            processUnitRepository.updateExecutionByExecutionNo(processUnitExecution);
        }
    }

    public void setThreadPoolExecutor(Executor threadPoolExecutor) {
        this.threadPoolExecutor = threadPoolExecutor;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }

    public Executor getThreadPoolExecutor() {
        return threadPoolExecutor;
    }
    
    


}
