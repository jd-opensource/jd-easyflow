package com.jd.easyflow.processunit.domain.service.impl.execution;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.domain.service.ProcessUnitExecutionPersistHandler;

/**
 * @author: chenzhenghao1
 */
public class ExecutionPersister implements ProcessUnitExecutionPersistHandler {
    
    private static final Logger log = LoggerFactory.getLogger(ExecutionPersister.class);


    private String defaultHandlerName = ProcessUnitConstants.THREAD_POOL_ASYNC_PERSIST_HANDLER_NAME;

    private Map<String, ProcessUnitExecutionPersistHandler> handlerMap;

    @Override
    public void persistBeforeCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        ProcessUnitExecutionPersistHandler handler = getHandler(context);
        if (handler == null) {
            log.warn("Execution Persist Handler not exists");
            return;
        }
        handler.persistBeforeCall(context, processUnitExecution);
    }

    @Override
    public void persistAfterCall(ExecContext context, ProcessUnitExecutionEntity processUnitExecution) {
        ProcessUnitExecutionPersistHandler handler = getHandler(context);
        if (handler == null) {
            log.warn("Execution Persist Handler not exists");
            return;
        }
        handler.persistAfterCall(context, processUnitExecution);
    }

    private ProcessUnitExecutionPersistHandler getHandler(ExecContext context) {
        if (handlerMap == null) {
            return null;
        }
        ProcessUnitEntity processUnit = context.getProcessUnit();
        String execType = context.getParam().getExecType();
        Map<String, Object> policyMap = (Map<String, Object>) processUnit
                .getConfig(ProcessUnitConstants.EXECUTION_PERSIST_POLICY);
        Map<String, Object> handlerConf = null;
        if (policyMap != null) {
            handlerConf = (Map<String, Object>) policyMap.get(execType);
            if (handlerConf == null) {
                handlerConf = (Map<String, Object>) policyMap.get(ProcessUnitConstants.EXECUTION_PERSIST_TYPE_DEFAULT);
            }
        }
        String handlerName = null;
        if (handlerConf != null) {
            handlerName = (String) handlerConf.get(ProcessUnitConstants.PU_PERSIST_HANDLER);
        }
        if (handlerName == null) {
            handlerName = defaultHandlerName;
        }

        return handlerMap.get(handlerName);
    }

    public String getDefaultHandlerName() {
        return defaultHandlerName;
    }

    public void setDefaultHandlerName(String defaultHandlerName) {
        this.defaultHandlerName = defaultHandlerName;
    }

    public Map<String, ProcessUnitExecutionPersistHandler> getHandlerMap() {
        return handlerMap;
    }

    public void setHandlerMap(Map<String, ProcessUnitExecutionPersistHandler> handlerMap) {
        this.handlerMap = handlerMap;
    }

}
