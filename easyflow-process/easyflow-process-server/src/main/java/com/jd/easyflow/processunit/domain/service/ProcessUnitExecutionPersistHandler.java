package com.jd.easyflow.processunit.domain.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;

/**
 * @author: chenzhenghao1
 */

public interface ProcessUnitExecutionPersistHandler {

    Logger logger = LoggerFactory.getLogger(ProcessUnitExecutionPersistHandler.class);

    void persistBeforeCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution);

    void persistAfterCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution);

}
