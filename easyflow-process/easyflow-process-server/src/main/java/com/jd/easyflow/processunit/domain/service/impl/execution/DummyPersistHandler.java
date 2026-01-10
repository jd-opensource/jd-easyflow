package com.jd.easyflow.processunit.domain.service.impl.execution;

import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.domain.service.ProcessUnitExecutionPersistHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public class DummyPersistHandler implements ProcessUnitExecutionPersistHandler {

    @Override
    public void persistBeforeCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        logger.info("DummyPersistHandler not persist.");
    }
    @Override
    public void persistAfterCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        logger.info("DummyPersistHandler not persist.");
    }
    
}
