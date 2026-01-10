package com.jd.easyflow.processunit.domain.service.impl;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.jd.easyflow.alert.AlertUtil;
import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.common.util.UUIDUtil;
import com.jd.easyflow.lock.Locker;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.gateway.ProcessUnitClientGateway;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.domain.model.vo.ExecPolicy;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.processunit.domain.support.LockManager;
import com.jd.easyflow.processunit.domain.support.PuTransactionTemplate;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 * 
 */
public class AsyncServerProcessUnitExecutor extends BaseProcessUnitExecutor {
    
    private static final Logger log = LoggerFactory.getLogger(AsyncServerProcessUnitExecutor.class);


    private static final String PU_EXECUTION = "PE";

    private LockManager lockManager;
    @Autowired
    private ProcessUnitRepository processUnitRepository;
    @Autowired
    private ProcessUnitClientGateway processUnitClientGateway;
    
    @Autowired(required = false)
    @Qualifier(ProcessUnitConstants.BEAN_PU_TX_TEMPLATE)
    private PuTransactionTemplate puTransactionTemplate = new PuTransactionTemplate();
    
    
    private Map<String, Object> defaultAlertRules;
    
    {
        defaultAlertRules = new HashMap<String, Object>();
        Map<String, Object> asyncResultConf = new HashMap<String, Object>();
        asyncResultConf.put("resultList", Arrays.asList(ProcessUnitConstants.RESULT_EXCEPTION, ProcessUnitConstants.RESULT_FAIL));
        defaultAlertRules.put(ProcessUnitConstants.ALERT_CODE_ASYNC_RESULT, asyncResultConf);
        Map<String, Object> asyncMaxTimesConf = new HashMap<String, Object>();
        asyncMaxTimesConf.put("resultList", Arrays.asList(ProcessUnitConstants.RESULT_EXCEPTION));
        defaultAlertRules.put(ProcessUnitConstants.ALERT_CODE_ASYNC_MAX_TIMES, asyncMaxTimesConf);        
    }
    
    
    @Override
    public ExecPolicy beforeCall(ExecContext context) {
        log.info("Start async before call");

        String instanceNo = context.getParam().getInstanceNo();
        String bizNo = context.getParam().getBizNo();
        String unitCode = context.getParam().getUnitCode();
        if (bizNo == null || unitCode == null) {
            ProcessUnitInstanceEntity instance = processUnitRepository.getInstance(instanceNo, null, null);
            bizNo = instance.getBizNo();
            unitCode = instance.getProcessUnitCode();
        }
        ProcessUnitEntity unit = processUnitRepository.getProcessUnitByCode(unitCode);
        Integer lockSeconds = (Integer) unit.getConfig(ProcessUnitConstants.CONF_LOCK_SECONDS);
        if (lockSeconds == null) {
            lockSeconds = ProcessUnitConstants.DEFAULT_LOCK_SECONDS;
        }
        String requestId = lockManager.lock(unitCode, bizNo, ProcessUnitConstants.SCENE_ASYNC);
        String finalBizNo = bizNo;
        String finalUnitCode = unitCode ;
        return puTransactionTemplate.doInTransaction(context.getProcessUnit(), context.getBizNo(), ProcessUnitConstants.SCENE_ASYNC, null, () -> {
        ProcessUnitInstanceEntity instance = processUnitRepository.getInstance(instanceNo, finalUnitCode, finalBizNo);
        context.setProcessUnit(unit);
        context.setInstance(instance);
        context.setLastResult(instance.getResult());
        context.setLastRequestContent(instance.getRequestContent());
        context.setLastResponseContent(instance.getResponseContent());
        context.setUnitCode(unit.getProcessUnitCode());
        context.setBizNo(instance.getBizNo());
        log.info("Unit code:{}, biz no:{}", context.getUnitCode(), context.getBizNo());
        context.setLocked(true);
        context.setLockRequestId(requestId);
        ExecPolicy policy = getExecPolicy(context);
        context.setPolicy(policy);
        saveBeforeCall(context);
        return policy;
        });
    }

    @Override
    public void call(ExecPolicy policy, ExecContext context) {
        switch (policy.getPolicyType()) {
        case ProcessUnitConstants.POLICY_EXCEPTION: {
            throw new UnsupportedOperationException();
        }
        case ProcessUnitConstants.POLICY_OLD: {
            throw new UnsupportedOperationException();
        }
        case ProcessUnitConstants.POLICY_REQUEST: {
            processUnitClientGateway.ayncCallReal(context);
            break;
        }
        default:
            throw new RuntimeException("policy illegal:" + policy);
        }
        List<String> alertResultList = getAlertResultList(ProcessUnitConstants.ALERT_CODE_ASYNC_RESULT, context.getProcessUnit());
        if (alertResultList != null && alertResultList.contains(context.getResult().getResult())) {
            AlertUtil.doAlert(ProcessUnitConstants.ALERT_CODE_ASYNC_RESULT,
                    "Process unit:" + context.getUnitCode() + " instance:" + context.getInstance().getInstanceNo() + "Async result trigger alert, Exec result:"
                            + context.getResult().getResult() + "Response content:" + context.getResult().getResponseContent(),
                    null, context.getInstance(), context.getInstance().getProductCode());
        }
        context.setExecuted(true);
    }
    
    
    private List<String> getAlertResultList(String alertCode, ProcessUnitEntity processUnit) {
        List<String> alertResultList = null;
        Map<String, Object> alertRules = processUnit == null ? null : (Map<String, Object>) processUnit.getConfig(ProcessUnitConstants.CONF_ALERT_RULES);
       if (alertRules != null) {
           Map<String, Object> alertConfig = (Map<String, Object>) alertRules.get(alertCode);
           if (alertConfig != null) {
               List<String> configResultList = (List<String>) alertConfig.get("resultList");
               if (configResultList != null) {
                   alertResultList = configResultList;
               }
           }
       }
       if (alertResultList == null && defaultAlertRules != null) {
               Map<String, Object> defaultAlertConfig = (Map<String, Object>) defaultAlertRules.get(alertCode);
               if (defaultAlertConfig != null) {
                   alertResultList = (List<String>) defaultAlertConfig.get("resultList");
               }
       }
       return alertResultList;
    }

    @Override
    public void afterCall(ExecContext context) {
        if (context.isExecuted()) {
            puTransactionTemplate.doInTransaction(context.getProcessUnit(), context.getBizNo(), ProcessUnitConstants.SCENE_ASYNC, null, () -> {
            ProcessUnitExecutionEntity execution = context.getExecution();
            execution.setResult(context.getResult().getResult());
            execution.setResponseContent(context.getResult().getResponseContent());
            execution.setResponseTime(new Date());
            execution.setElaspeTime(
                    (int) (execution.getResponseTime().getTime() - execution.getRequestTime().getTime()));
            String extData = execution.getExtData();
            Map<String, Object> executionExtDataMap = JSON.parseObject(extData, Map.class);
            if (executionExtDataMap == null) {
                executionExtDataMap = new HashMap<>();
            }
            executionExtDataMap.put(ProcessUnitConstants.EXECUTION_EXT_DATA_AFTER_VARS, context.getVariables());
            executionExtDataMap.put(ProcessUnitConstants.EXECUTION_EXT_DATA_CLIENT_INFO, context.getClientInfo());
            execution.setExtData(JSON.toJSONString(executionExtDataMap));
            processUnitRepository.updateExecution(execution);
            ProcessUnitInstanceEntity instance = context.getInstance();
            instance.setResult(context.getResult().getResult());
            instance.setResponseContent(context.getResult().getResponseContent());
            instance.setAutoRunTimes(instance.getAutoRunTimes() == null ? 1 : instance.getAutoRunTimes() + 1);

            Boolean autoRunFlag = context.getResult().getAutoRunFlag();
            if (autoRunFlag == null) {
                autoRunFlag = false;
                Integer maxTimes = (Integer) context.getProcessUnit()
                        .getConfig(ProcessUnitConstants.CONF_AUTO_ASYNC_MAX_TIMES);
                if (maxTimes == null || instance.getAutoRunTimes() >= maxTimes) {
                    autoRunFlag = false;
                    List<String> alertResultList = getAlertResultList(ProcessUnitConstants.ALERT_CODE_ASYNC_MAX_TIMES, context.getProcessUnit());
                    if (alertResultList != null && alertResultList.contains(context.getResult().getResult())) {
                        AlertUtil.doAlert(ProcessUnitConstants.ALERT_CODE_ASYNC_MAX_TIMES,
                                "Process unit:" + context.getUnitCode() + " instance:" + instance.getInstanceNo()
                                        + "retry to max times!!!, exec result:" + context.getResult().getResult() + "response content:"
                                        + context.getResult().getResponseContent(),
                                null, instance, instance.getProductCode());
                    }
                } else {
                    Map<String, Object> autoRunPolicy = (Map<String, Object>) context.getProcessUnit()
                            .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_POLICY);
                    if (autoRunPolicy != null) {
                        autoRunFlag = (Boolean) autoRunPolicy.get(context.getResult().getResult());
                        if (autoRunFlag == null) {
                            autoRunFlag = false;
                        }
                    }
                }
            }
            instance.setAutoRunFlag(autoRunFlag);
            
            if (context.getResult().getNextAutoRunTime() != null) {
                instance.setNextAutoRunTime(context.getResult().getNextAutoRunTime());
            } else {
                Integer autoAsyncRunPeriod = (Integer) context.getProcessUnit()
                        .getConfig(ProcessUnitConstants.CONF_AUTO_ASYNC_RUN_PERIOD);
                if (autoAsyncRunPeriod == null) {
                    throw new EasyFlowException("autoAsyncRunPeriod is null");
                }
                if (Boolean.TRUE.equals(instance.getAutoRunFlag())) {
                    Integer powerMaxTime = (Integer) context.getProcessUnit()
                            .getConfig(ProcessUnitConstants.CONF_AUTO_ASYNC_RUN_PERIOD_POWER_MAX_TIME);
                    if (powerMaxTime == null) {
                        instance.setNextAutoRunTime(new Date(System.currentTimeMillis() + autoAsyncRunPeriod * 1000L));
                    } else {
                        int autoRunTimes = instance.getAutoRunTimes();
                        int powerTime = powerMaxTime < autoRunTimes ? powerMaxTime : autoRunTimes;
                        int currentPeriod = (int) (autoAsyncRunPeriod * Math.pow(2, powerTime));
                        if (log.isDebugEnabled()) {
                            log.debug("autoRunTimes:{} powerMaxTime:{} powerTime:{} currentPeriod:{}",
                                    instance.getAutoRunTimes(), powerMaxTime, powerTime, currentPeriod);
                        }
                        instance.setNextAutoRunTime(new Date(System.currentTimeMillis() + currentPeriod * 1000L));
                    }
                }
            }
            Map<String, String> variables = context.getVariables();
            instance.setVars(JSON.toJSONString(variables));
            log.info("Instance update result:{}", instance);
            processUnitRepository.updateInstance(instance);
            return null;
            });
        }
        if (context.isLocked()) {
            lockManager.unlock(context.getProcessUnit().getProcessUnitCode(), context.getBizNo(), ProcessUnitConstants.SCENE_ASYNC, context.getLockRequestId());
        }
    }

    protected void saveBeforeCall(ExecContext context) {
        ProcessUnitInstanceEntity instance = context.getInstance();
        instance.setResult(ProcessUnitConstants.RESULT_UNKNOWN);
        instance.setResponseContent(null);
        processUnitRepository.updateInstance(instance);
        ProcessUnitExecutionEntity execution = new ProcessUnitExecutionEntity();
        execution.setInstanceNo(context.getInstance().getInstanceNo());
        execution.setRequestContent(context.getInstance().getRequestContent());
        execution.setRequestTime(new Date());
        execution.setRequestNo(UUIDUtil.getSimpleUUID());
        execution.setProcessUnitCode(context.getUnitCode());
        execution.setBizNo(context.getInstance().getBizNo());
        execution.setExecutionNo(CodeGenerateHelper.generateCode("PU_EXECUTION", PU_EXECUTION));
        execution.setExecType(context.getParam().getExecType());
        execution.setProductCode(instance.getProductCode());
        Map<String, Object> extData = new HashMap<String, Object>();
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_POLICY_TYPE, context.getPolicy().getPolicyType());
        Map<String, Object> requestContext = context.getParam().getRequestContext();
        if (requestContext != null && ! requestContext.isEmpty()) {
            extData.put(ProcessUnitConstants.EXECUTION_REQUEST_CONTEXT, requestContext);
        }
        if (instance.getExtData() != null) {
            Map<String, Object> extDataMap = JSON.parseObject(instance.getExtData(), Map.class);
            extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_BEFORE_VARS,
                    JSON.parseObject(instance.getVars(), Map.class));
        }
        execution.setExtData(JSON.toJSONString(extData));
        processUnitRepository.saveExecution(execution);
        context.setExecution(execution);
        log.info("Execution saved, executionNo:" + execution.getExecutionNo());
    }

    protected ExecPolicy getExecPolicy(ExecContext context) {
        ExecPolicy policy = new ExecPolicy();
        String result = context.getLastResult();
        Map<String, String> execPolicy = (Map<String, String>) context.getProcessUnit()
                .getConfig(ProcessUnitConstants.CONF_ASYNC_EXEC_POLICY);
        String policyType = null;
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
                log.info("Exception policy:" + policyType);
            }
        }
        policy.setPolicyType(policyType);
        return policy;
    }
    
    public Map<String, Object> getDefaultAlertRules() {
        return defaultAlertRules;
    }

    public void setDefaultAlertRules(Map<String, Object> defaultAlertRules) {
        this.defaultAlertRules = defaultAlertRules;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }

    public ProcessUnitClientGateway getProcessUnitClientGateway() {
        return processUnitClientGateway;
    }

    public void setProcessUnitClientGateway(ProcessUnitClientGateway processUnitClientGateway) {
        this.processUnitClientGateway = processUnitClientGateway;
    }

    public LockManager getLockManager() {
        return lockManager;
    }

    public void setLockManager(LockManager lockManager) {
        this.lockManager = lockManager;
    }

    

}
