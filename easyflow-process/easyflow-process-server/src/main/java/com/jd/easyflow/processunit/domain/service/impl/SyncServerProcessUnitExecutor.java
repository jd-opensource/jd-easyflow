package com.jd.easyflow.processunit.domain.service.impl;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.domain.model.vo.ExecPolicy;
import com.jd.easyflow.processunit.domain.model.vo.ExecResult;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.processunit.domain.service.impl.execution.ExecutionPersister;
import com.jd.easyflow.processunit.domain.support.LockManager;
import com.jd.easyflow.processunit.domain.support.PuTransactionTemplate;
import com.jd.easyflow.utils.json.JSON;

/**
 *
 * @author liyuliang5
 *
 */
public class SyncServerProcessUnitExecutor extends BaseProcessUnitExecutor {
    
    private static final Logger log = LoggerFactory.getLogger(SyncServerProcessUnitExecutor.class);


    private static final String PU_EXECUTION = "PE";
    private static final String PU_INSTANCE = "PI";

    private LockManager lockManager;
    @Autowired
    private ProcessUnitRepository processUnitRepository;

    @Autowired
    private ExecutionPersister executionPersister;

    private int syncAutoRunDelaySeconds = 10;
    
    @Autowired(required = false)
    @Qualifier(ProcessUnitConstants.BEAN_PU_TX_TEMPLATE)
    private PuTransactionTemplate puTransactionTemplate = new PuTransactionTemplate();

    @Override
    public ExecPolicy beforeCall(ExecContext context) {
        ProcessUnitEntity unit = processUnitRepository.getProcessUnitByCode(context.getUnitCode());
        context.setProcessUnit(unit);
        if (ProcessUnitConstants.STATUS_INVALID.equals(unit.getStatus())) {
            log.info(" unit illegal, return REQUEST");
            return new ExecPolicy(ProcessUnitConstants.POLICY_REQUEST);
        }
        ExecPolicy policy = null;
        String requestId = lockManager.lock(unit.getProcessUnitCode(), context.getBizNo(), ProcessUnitConstants.SCENE_SYNC);
        try {
            context.setLockRequestId(requestId);
            saveInstanceBeforeCall(context);
            policy = getExecPolicy(context);
            context.setPolicy(policy);
            saveExecutionBeforeCall(context);
        } catch (Exception exception) {
            log.error("Process unit sync before call exception, " + exception.getMessage(), exception);
            lockManager.unlock(unit.getProcessUnitCode(), context.getBizNo(), ProcessUnitConstants.SCENE_SYNC, requestId);
            throw exception;
        }
        return policy;
    }

    @Override
    public void afterCall(ExecContext context) {
        if (ProcessUnitConstants.STATUS_INVALID.equals(context.getProcessUnit().getStatus())) {
            return;
        }
        puTransactionTemplate.doInTransaction(context.getProcessUnit(), context.getBizNo(), ProcessUnitConstants.SCENE_SYNC, null, () -> {
            afterCallOperation(context);
            return null;
        });
    }
    
    private void afterCallOperation(ExecContext context) {
        ExecResult result = context.getResult();
        String executionNo = context.getExecutionNo();
        ProcessUnitExecutionEntity execution = new ProcessUnitExecutionEntity();
        execution.setExecutionNo(executionNo);
        execution.setResult(result.getResult());
        execution.setResponseContent(result.getResponseContent());
        execution.setResponseTime(new Date());
        if (context.getRequestTime() != 0) {
            execution.setElaspeTime((int) (System.currentTimeMillis() - context.getRequestTime()));
        }
        execution.setProcessUnitCode(context.getUnitCode());
        execution.setBizNo(context.getBizNo());
        execution.setInstanceNo(context.getInstanceNo());
        executionPersister.persistAfterCall(context, execution);
        String instanceNo = context.getInstanceNo();
        ProcessUnitInstanceEntity instance = new ProcessUnitInstanceEntity();
        instance.setInstanceNo(instanceNo);
        instance.setProcessUnitCode(context.getUnitCode());
        instance.setBizNo(context.getBizNo());
        instance.setResult(result.getResult());
        instance.setResponseContent(result.getResponseContent());
        Map<String, Object> autoRunPolicy = (Map<String, Object>) context.getProcessUnit()
                .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_POLICY);
        Boolean autoRunFlag = context.getResult().getAutoRunFlag();
        if (autoRunFlag == null) {
            autoRunFlag = false;
            if (autoRunPolicy != null) {
                autoRunFlag = Boolean.TRUE.equals(autoRunPolicy.get(context.getResult().getResult()));
            }
        }
        instance.setAutoRunFlag(autoRunFlag);
        if (result.getNextAutoRunTime() != null) {
           instance.setNextAutoRunTime(result.getNextAutoRunTime());
        }
        if (autoRunFlag && instance.getNextAutoRunTime() == null) {
            instance.setNextAutoRunTime(new Date());
        }
        instance.setVars(JSON.toJSONString(context.getVariables()));
        instance.setExtData(context.getExtDataStr());
        processUnitRepository.updateInstanceByInstanceNoSelective(instance);
        if (log.isDebugEnabled()) {
            log.debug("Process unit instance update success:{}", instance);
        }

        String lockRequestId = context.getLockRequestId();
        if (lockRequestId == null) {
            ProcessUnitExecutionEntity contextExecution = context.getExecution();
            Map<String, Object> extData = JSON.parseObject(contextExecution.getExtData(), Map.class);
            lockRequestId = (String) extData.get(ProcessUnitConstants.EXECUTION_EXT_DATA_LOCK_REQUEST_ID);
        }
        lockManager.unlock(context.getProcessUnit().getProcessUnitCode(), context.getBizNo(), ProcessUnitConstants.SCENE_SYNC,lockRequestId);
    }

    protected ExecPolicy getExecPolicy(ExecContext context) {
        ExecPolicy policy = new ExecPolicy();
        String result = context.getLastResult();
        Map<String, String> execPolicy = (Map<String, String>) context.getProcessUnit()
                .getConfig(ProcessUnitConstants.CONF_SYNC_EXEC_POLICY);
        String policyType = null;
        if (context.isNewInstance()) {
            policyType = execPolicy != null ? execPolicy.get(ProcessUnitConstants.REQ_TYPE_NEW)
                    : ProcessUnitConstants.POLICY_REQUEST;
        } else {
            if (ProcessUnitConstants.RESULT_SUCCESS.equals(result)) {
                policyType = execPolicy != null ? execPolicy.get(ProcessUnitConstants.REQ_TYPE_SUCCESS)
                        : ProcessUnitConstants.POLICY_EXCEPTION;
            } else if (ProcessUnitConstants.RESULT_FAIL.equals(result)) {
                policyType = execPolicy != null ? execPolicy.get(ProcessUnitConstants.REQ_TYPE_FAIL)
                        : ProcessUnitConstants.POLICY_EXCEPTION;
            } else if (ProcessUnitConstants.RESULT_DOING.equals(result)) {
                policyType = execPolicy != null ? execPolicy.get(ProcessUnitConstants.REQ_TYPE_DOING)
                        : ProcessUnitConstants.POLICY_EXCEPTION;
            } else {
                if (ProcessUnitConstants.RESULT_NONE.equals(result) && execPolicy != null) {
                    policyType = execPolicy.get(ProcessUnitConstants.RESULT_NONE);
                } else if (ProcessUnitConstants.RESULT_UNKNOWN.equals(result) && execPolicy != null) {
                    policyType = execPolicy.get(ProcessUnitConstants.RESULT_UNKNOWN);
                }
                if (policyType == null) {
                    policyType = execPolicy != null ? execPolicy.get(ProcessUnitConstants.REQ_TYPE_EXCEPTION)
                            : ProcessUnitConstants.POLICY_EXCEPTION;
                    log.info("Excetpion policy:" + policyType);
                }
            }
            policy.setResult(context.getLastResult());
            policy.setRequestContent(context.getLastRequestContent());
            policy.setResponseContent(context.getLastResponseContent());
        }
        policy.setPolicyType(policyType);
        return policy;
    }

    protected void saveInstanceBeforeCall(ExecContext context) {
        puTransactionTemplate.doInTransaction(context.getProcessUnit(), context.getBizNo(), ProcessUnitConstants.SCENE_SYNC, null, () -> {
            saveInstanceBeforeCallOperation(context);
            return null;
        });
    }
    
    private void saveInstanceBeforeCallOperation(ExecContext context) {
        ProcessUnitInstanceEntity instance = processUnitRepository
                .getInstanceByUnitCodeAndBizNo(context.getProcessUnit().getProcessUnitCode(), context.getBizNo());

        if (instance == null) {
            context.setLastResult(null);
            context.setLastRequestContent(null);
            context.setLastResponseContent(null);
            instance = new ProcessUnitInstanceEntity();
            instance.setBizNo(context.getBizNo());
            instance.setInstanceNo(CodeGenerateHelper.generateCode("PU_INSTANCE", "PI"));
            instance.setProcessUnitCode(context.getProcessUnit().getProcessUnitCode());
            instance.setRequestContent(context.getParam().getRequestContent());
            instance.setResult(ProcessUnitConstants.RESULT_UNKNOWN);
            instance.setProductCode(context.getParam().getProductCode());
            instance.setParentNo(context.getParentNo());
            context.setNewInstance(true);
            Map<String, Object> autoRunPolicy = (Map<String, Object>) context.getProcessUnit()
                    .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_POLICY);
            Boolean autoRunFlag = false;
            if (autoRunPolicy != null) {
                autoRunFlag = (Boolean) autoRunPolicy.get(ProcessUnitConstants.RESULT_UNKNOWN);
                if (autoRunFlag == null) {
                    autoRunFlag = false;
                }
            }
            instance.setAutoRunFlag(autoRunFlag);
            if (autoRunFlag) {
                Integer autoRunDelaySeconds = (Integer) context.getProcessUnit()
                        .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_DELAY_SECONDS);
                int delaySeconds = autoRunDelaySeconds == null ? syncAutoRunDelaySeconds : autoRunDelaySeconds;
                instance.setNextAutoRunTime(new Date(System.currentTimeMillis() + delaySeconds * 1000));
            }
            Map<String, Object> extDataMap = new HashMap<>();
            Map<String, String> clientInfo = context.getClientInfo();
            if (clientInfo != null) {
                extDataMap.put(ProcessUnitConstants.INSTANCE_EXT_DATA_CREATE_CLIENT_INFO, clientInfo);
            }
            if (extDataMap.size() > 0) {
                instance.setExtData(JSON.toJSONString(extDataMap));
            }
            processUnitRepository.saveInstance(instance);
            if (log.isDebugEnabled()) {
                log.debug("New process unit instance created:{}", instance);
            } else if (log.isInfoEnabled()) {
                log.info("New process unit instance created, instanceNo:" + instance.getInstanceNo());
            }

        } else {
            context.setLastResult(instance.getResult());
            context.setLastRequestContent(instance.getRequestContent());
            context.setLastResponseContent(instance.getResponseContent());
            Map<String, Object> autoRunPolicy = (Map<String, Object>) context.getProcessUnit()
                    .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_POLICY);
            Boolean autoRunFlag = false;
            if (autoRunPolicy != null) {
                autoRunFlag = (Boolean) autoRunPolicy.get(ProcessUnitConstants.RESULT_UNKNOWN);
                if (autoRunFlag == null) {
                    autoRunFlag = false;
                }
            }
            instance.setAutoRunFlag(autoRunFlag);
            if (autoRunFlag) {
                Integer autoRunDelaySeconds = (Integer) context.getProcessUnit()
                        .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_DELAY_SECONDS);
                int delaySeconds = autoRunDelaySeconds == null ? syncAutoRunDelaySeconds : autoRunDelaySeconds;
                instance.setNextAutoRunTime(new Date(System.currentTimeMillis() + delaySeconds * 1000L));
            }
            instance.setResult(ProcessUnitConstants.RESULT_UNKNOWN);
            instance.setResponseContent(null);
            instance.setRequestContent(context.getParam().getRequestContent());
            processUnitRepository.updateInstance(instance);

        }
        context.setInstanceNo(instance.getInstanceNo());
        context.setInstance(instance);
    }

    protected void saveExecutionBeforeCall(ExecContext context) {
        String executionNo = CodeGenerateHelper.generateCode("PU_EXECUTION", PU_EXECUTION);
        context.setExecutionNo(executionNo);
        ProcessUnitExecutionEntity execution = new ProcessUnitExecutionEntity();
        execution.setExecutionNo(executionNo);
        execution.setInstanceNo(context.getInstance().getInstanceNo());
        execution.setProductCode(context.getInstance().getProductCode());
        execution.setProcessUnitCode(context.getProcessUnit().getProcessUnitCode());
        execution.setBizNo(context.getInstance().getBizNo());
        execution.setRequestNo(context.getParam().getRequestNo());
        execution.setRequestContent(context.getParam().getRequestContent());
        execution.setExecType(ProcessUnitConstants.EXEC_TYPE_SYNC);
        execution.setRequestTime(new Date());
        Map<String, Object> extData = new HashMap<String, Object>();
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_LOCK_REQUEST_ID, context.getLockRequestId());
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_POLICY_TYPE, context.getPolicy().getPolicyType());
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_CLIENT_INFO, context.getClientInfo());
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_BEFORE_VARS, JSON.parseObject(context.getInstance().getVars(), Map.class));
        execution.setExtData(JSON.toJSONString(extData));
        executionPersister.persistBeforeCall(context, execution);
        context.setExecution(execution);
    }

    public int getSyncAutoRunDelaySeconds() {
        return syncAutoRunDelaySeconds;
    }

    public void setSyncAutoRunDelaySeconds(int syncAutoRunDelaySeconds) {
        this.syncAutoRunDelaySeconds = syncAutoRunDelaySeconds;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }

    public ExecutionPersister getExecutionPersister() {
        return executionPersister;
    }

    public void setExecutionPersister(ExecutionPersister executionPersister) {
        this.executionPersister = executionPersister;
    }

    public PuTransactionTemplate getPuTransactionTemplate() {
        return puTransactionTemplate;
    }

    public void setPuTransactionTemplate(PuTransactionTemplate puTransactionTemplate) {
        this.puTransactionTemplate = puTransactionTemplate;
    }

    public LockManager getLockManager() {
        return lockManager;
    }

    public void setLockManager(LockManager lockManager) {
        this.lockManager = lockManager;
    }
    
}
