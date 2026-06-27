package com.jd.easyflow.processunit.domain.service;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.jd.easyflow.alert.AlertUtil;
import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.common.util.UUIDUtil;
import com.jd.easyflow.lock.Locker;
import com.jd.easyflow.message.MessageSendService;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateReq;
import com.jd.easyflow.processunit.domain.constant.ContextDataKeys;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.model.converter.ProcessUnitExecuteConverter;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.domain.model.enums.ProcessUnitErrorCodeEnum;
import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteReq;
import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteRes;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.domain.model.vo.ExecParam;
import com.jd.easyflow.processunit.domain.model.vo.ExecPolicy;
import com.jd.easyflow.processunit.domain.model.vo.ExecResult;
import com.jd.easyflow.processunit.domain.model.vo.ExecuteReq;
import com.jd.easyflow.processunit.domain.model.vo.ExecuteRes;
import com.jd.easyflow.processunit.domain.model.vo.ProcessUnitExecuteMessage;
import com.jd.easyflow.processunit.domain.model.vo.ProcessUnitInstanceKey;
import com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO;
import com.jd.easyflow.processunit.domain.model.vo.SyncAfterCallReq;
import com.jd.easyflow.processunit.domain.model.vo.SyncAfterCallRes;
import com.jd.easyflow.processunit.domain.model.vo.SyncBeforeCallReq;
import com.jd.easyflow.processunit.domain.model.vo.SyncBeforeCallRes;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.processunit.domain.service.impl.AsyncServerProcessUnitExecutor;
import com.jd.easyflow.processunit.domain.service.impl.SyncServerProcessUnitExecutor;
import com.jd.easyflow.processunit.domain.support.LockManager;
import com.jd.easyflow.processunit.domain.support.PuTransactionTemplate;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitService.class);


    private static final String PU_EXECUTION = "PE";

    private String executeMessageTopic;

    private String createMessageTopic;

    private String updateMessageTopic;
    @Autowired
    private AsyncServerProcessUnitExecutor asyncServerProcessUnitExecutor;
    @Autowired
    private ProcessUnitRepository processUnitRepository;
    private MessageSendService messageSendService;
    @Autowired
    private SyncServerProcessUnitExecutor syncServerProcessUnitExecutor;
    
    private LockManager lockManager;
    
    private int asyncMaxCount = 100000;
    
    private int pagerQueryTimeRangeDays = 30;
    
    private int pagerQueryMaxPageSize = 10000;
    
    private int maxThreadNumPerUnitCode = 1000;
    
    
    @Autowired(required = false)
    @Qualifier(ProcessUnitConstants.BEAN_PU_TX_TEMPLATE)
    private PuTransactionTemplate puTransactionTemplate = new PuTransactionTemplate();
    
    private volatile boolean stop = false;
    

    public String create(ProcessUnitCreateReq req) {
        ProcessUnitEntity unit = processUnitRepository.getProcessUnitByCode(req.getUnitCode());
        Map<String, Object> createExecPolicy = (Map<String, Object>) unit
                .getConfig(ProcessUnitConstants.CONF_CREATE_EXEC_POLICY);
        Boolean asyncCreate =createExecPolicy == null ? false : (Boolean) createExecPolicy.get("asyncCreate");
        if (!Boolean.TRUE.equals(asyncCreate)) {
            return syncCreate(req, unit);
        } else {
            asyncCreate(req, unit);
        }
        return null;
    }

    public String syncCreate(ProcessUnitCreateReq req, ProcessUnitEntity unit) {
        log.info("Sync create:{}", JSON.toJSONString(req));
        if (unit == null) {
            unit = processUnitRepository.getProcessUnitByCode(req.getUnitCode());
        }
        ExecParam param = ProcessUnitExecuteConverter.INSTANCE.convert(req);
        ExecContext context = new ExecContext(param);
        context.setUnitCode(param.getUnitCode());
        context.setBizNo(param.getBizNo());
        context.setProcessUnit(unit);
        context.setClientInfo(req.getClientInfo());

        createIfNotExists(context);
        afterCreate(context);
        return context.getInstance().getInstanceNo();
    }

    private void asyncCreate(ProcessUnitCreateReq req, ProcessUnitEntity unit) {
        String reqString = JSON.toJSONString(req);
        log.info("asyncCreate:{}", reqString);
        messageSendService.sendMessage(UUIDUtil.getSimpleUUID(), createMessageTopic, reqString);
    }

    private void afterCreate(ExecContext context) {
        Map<String, Object> createExecPolicy = (Map<String, Object>) context.getProcessUnit()
                .getConfig(ProcessUnitConstants.CONF_CREATE_EXEC_POLICY);
        String policy = createExecPolicy == null ? null : (String) createExecPolicy.get("policy");
        if (policy == null) {
            return;
        }
        if ("asyncExecAfterSave".equals(policy) || "asyncExecAfterCommit".equals(policy)) {
            ProcessUnitExecuteMessage msg = new ProcessUnitExecuteMessage();
            msg.setInstanceNo(context.getInstance().getInstanceNo());
            msg.setUnitCode(context.getInstance().getProcessUnitCode());
            msg.setBizNo(context.getInstance().getBizNo());
            log.info("process unit send create MQ:{}", JSON.toJSONString(msg));
            String puExecuteMessageTopic = (String) context.getProcessUnit()
                    .getConfig(ProcessUnitConstants.CONF_EXECUTE_MESSAGE_TOPIC);
            if (puExecuteMessageTopic == null) {
                puExecuteMessageTopic = executeMessageTopic;
            }
            if ("asyncExecAfterSave".equals(policy)) {
                messageSendService.sendMessage(context.getBizNo(), puExecuteMessageTopic, JSON.toJSONString(msg));
            } else {
                String finalPuExecuteMessageTopic = puExecuteMessageTopic;
                TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        messageSendService.sendMessage(context.getBizNo(), finalPuExecuteMessageTopic, JSON.toJSONString(msg));
                    }
                });
            }
        } else if ("syncExecAfterSave".equals(policy) || "syncExecAfterCommit".equals(policy)) {
            ExecParam param = new ExecParam();
            param.setInstanceNo(context.getInstance().getInstanceNo());
            param.setUnitCode(context.getInstance().getProcessUnitCode());
            param.setBizNo(context.getInstance().getBizNo());
            param.setExecType(ProcessUnitConstants.EXEC_TYPE_AFTER_CREATE);
            if ("syncExecAfterSave".equals(policy)) {
                log.info("Process unit execute start");
                ExecResult result = asyncServerProcessUnitExecutor.execute(param);
                ExecuteRes res = new ExecuteRes(result.getResult(), result.getResponseContent());
                log.info("Process unit execute end:{}", res);
            } else {
                TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        log.info("Process unit execute start");
                        ExecResult result = asyncServerProcessUnitExecutor.execute(param);
                        ExecuteRes res = new ExecuteRes(result.getResult(), result.getResponseContent());
                        log.info("Process unit execute end:{}", res);
                    }
                }); 
            }
        } else {
            throw new IllegalArgumentException(policy);
        }
    }

    public void update(ProcessUnitUpdateReq req) {
        ProcessUnitEntity unit = processUnitRepository.getProcessUnitByCode(req.getUnitCode());
        Map<String, Object> updateExecPolicy = (Map<String, Object>) unit
                .getConfig(ProcessUnitConstants.CONF_UPDATE_EXEC_POLICY);
        Boolean asyncUpdate = updateExecPolicy == null ? false : (Boolean) updateExecPolicy.get("asyncUpdate");
        if (!Boolean.TRUE.equals(asyncUpdate)) {
            doUpdate(req);
        } else {
            String reqString = JSON.toJSONString(req);
            log.info("Process unit send update message:{}", reqString);
            messageSendService.sendMessage(UUIDUtil.getSimpleUUID(), updateMessageTopic, reqString);
        }
    }

    public void doUpdate(ProcessUnitUpdateReq req) {
        if (req.getInstanceNo() == null && (req.getUnitCode() == null || req.getBizNo() == null)) {
            throw new EasyFlowException("Both bizNo and instanceNo are null");
        }
        String bizNo = req.getBizNo();
        String unitCode = req.getUnitCode();
        ProcessUnitInstanceEntity instance = null;
        if (bizNo == null || unitCode == null) {
            instance = processUnitRepository.getInstance(req.getInstanceNo(), unitCode, bizNo);
            bizNo = instance.getBizNo();
            unitCode = instance.getProcessUnitCode();
        }
        final String finalUnitCode = unitCode;
        final String finalBizNo = bizNo;
        ProcessUnitEntity unit = processUnitRepository.getProcessUnitByCode(req.getUnitCode());
        if (ProcessUnitConstants.STATUS_INVALID.equals(unit.getStatus())) {
            return;
        }
        if (Boolean.FALSE.equals(req.getLock())) {
            puTransactionTemplate.doInTransaction(unit, finalBizNo, ProcessUnitConstants.SCENE_UPDATE, null, () -> {
                doUpdateOperation(req, unit, finalUnitCode, finalBizNo);
                return null;
            });
        } else {
            lockManager.doInlock(unitCode, bizNo, ProcessUnitConstants.SCENE_UPDATE, () -> {
                        puTransactionTemplate.doInTransaction(unit, finalBizNo, null, null, () -> {
                            doUpdateOperation(req, unit, finalUnitCode, finalBizNo);
                            return null;
                        });
                        return null;
                    });
        }
    }
    
    private void doUpdateOperation(ProcessUnitUpdateReq req, ProcessUnitEntity unit, String unitCode, String bizNo) {
        ProcessUnitInstanceEntity processUnitInstance = processUnitRepository
                .getInstanceByUnitCodeAndBizNo(unitCode, bizNo);
        String beforeVars = processUnitInstance.getVars();
        Map<String, String> variables = req.getVariables();
        if (ProcessUnitUpdateReq.UPDATE_TYPE_VARIABLES.equals(req.getUpdateType())) {
            processUnitInstance.setVars(JSON.toJSONString(req.getVariables()));
        } else if (ProcessUnitUpdateReq.UPDATE_TYPE_REQUEST_CONTENT.equals(req.getUpdateType())) {
            processUnitInstance.setRequestContent(req.getRequestContent());
        } else {

            processUnitInstance.setResult(req.getResult());
            processUnitInstance.setResponseContent(req.getResponseContent());

            Boolean autoRunFlag = req.getAutoRunFlag();
            if (autoRunFlag == null) {
                autoRunFlag = false;
                Map<String, Object> autoRunPolicy = (Map<String, Object>) unit
                        .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_POLICY);
                if (autoRunPolicy != null) {
                    autoRunFlag = (Boolean) autoRunPolicy.get(processUnitInstance.getResult());
                    if (autoRunFlag == null) {
                        autoRunFlag = false;
                    }
                }
            }
            processUnitInstance.setAutoRunFlag(autoRunFlag);

            if (autoRunFlag) {
                if (req.getNextAutoRunTime() != null) {
                    processUnitInstance.setNextAutoRunTime(req.getNextAutoRunTime());
                } else {
                    if (processUnitInstance.getNextAutoRunTime() == null) {
                        Date nextAutoRunTime = null;
                        Integer autoRunDelaySeconds = (Integer) unit
                                .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_DELAY_SECONDS);
                        if (autoRunDelaySeconds == null) {
                            nextAutoRunTime = new Date();
                        } else {
                            nextAutoRunTime = new Date(System.currentTimeMillis() + autoRunDelaySeconds * 1000);
                        }
                        processUnitInstance.setNextAutoRunTime(nextAutoRunTime);
                    }
                }
            }
            processUnitInstance.setVars(JSON.toJSONString(variables));
        }
        log.info("Process unit update result :{}", processUnitInstance);
        processUnitRepository.updateInstance(processUnitInstance);

        ProcessUnitExecutionEntity execution = new ProcessUnitExecutionEntity();
        execution.setInstanceNo(processUnitInstance.getInstanceNo());
        execution.setProductCode(processUnitInstance.getProductCode());
        execution.setRequestContent(req.getRequestContent() != null ? req.getRequestContent()
                : processUnitInstance.getRequestContent());
        execution.setRequestTime(req.getRequestTime());
        execution.setResponseTime(req.getResponseTime());
        if (req.getRequestTime() != null && req.getResponseTime() != null) {
            execution.setElaspeTime(
                    (int) (req.getResponseTime().getTime() - req.getRequestTime().getTime()));
        }
        execution.setRequestNo(req.getRequestNo());
        execution.setProcessUnitCode(unit.getProcessUnitCode());
        execution.setBizNo(bizNo);
        execution.setExecutionNo(CodeGenerateHelper.generateCode("PU_EXECUTION", PU_EXECUTION));
        execution.setExecType(req.getExecType());
        execution.setResult(req.getResult());
        execution.setResponseContent(req.getResponseContent());
        Map<String, Object> extData = new HashMap<String, Object>();
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_POLICY_TYPE,
                ProcessUnitConstants.POLICY_REQUEST);
        if (req.getRequestContext() != null) {
            extData.put(ProcessUnitConstants.EXECUTION_REQUEST_CONTEXT, req.getRequestContext());
        }
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_CALL_TYPE,
                ProcessUnitConstants.CALL_TYPE_UPDATE);
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_BEFORE_VARS, JSON.parseObject(beforeVars, Map.class));
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_AFTER_VARS, variables);
        extData.put(ProcessUnitConstants.EXECUTION_EXT_DATA_CLIENT_INFO, req.getClientInfo());
        execution.setExtData(JSON.toJSONString(extData));
        processUnitRepository.saveExecution(execution);
        log.info("Execuiton saved is :" + execution.getExecutionNo());
    }

    private void createIfNotExists(ExecContext context) {
        lockManager.doInlock(context.getUnitCode(), context.getBizNo(), ProcessUnitConstants.SCENE_CREATE, () -> {
                    puTransactionTemplate.doInTransaction(context.getProcessUnit(), context.getBizNo(), ProcessUnitConstants.SCENE_CREATE, null, () -> {
                        createIfNotExistsOperation(context);
                        return null;
                    });
            return null;
        });
    }
    
    private void createIfNotExistsOperation(ExecContext context) {
        ProcessUnitInstanceEntity instance = processUnitRepository
                .getInstanceByUnitCodeAndBizNo(context.getProcessUnit().getProcessUnitCode(), context.getBizNo());
        if (instance == null) {
            instance = new ProcessUnitInstanceEntity();
            instance.setBizNo(context.getBizNo());
            instance.setInstanceNo(CodeGenerateHelper.generateCode("PU_INSTANCE", "PI"));
            instance.setParentNo(context.getParentNo());
            instance.setProcessUnitCode(context.getProcessUnit().getProcessUnitCode());
            instance.setRequestContent(context.getParam().getRequestContent());
            instance.setProductCode(context.getParam().getProductCode());
            instance.setResult(ProcessUnitConstants.RESULT_NONE);
            Map<String, Object> autoRunPolicy = (Map<String, Object>) context.getProcessUnit()
                    .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_POLICY);
            Boolean autoRunFlag = context.getParam().getAutoRunFlag();
            if (autoRunFlag == null) {
                autoRunFlag = autoRunPolicy != null && Boolean.TRUE.equals(autoRunPolicy.get(ProcessUnitConstants.RESULT_NONE));
            }
            instance.setAutoRunFlag(autoRunFlag);
            if (autoRunFlag) {
                Date nextAutoRunTime = context.getParam().getNextAutoRunTime();
                if (nextAutoRunTime == null) {
                    Integer autoRunDelaySeconds = (Integer) context.getProcessUnit()
                            .getConfig(ProcessUnitConstants.CONF_AUTO_RUN_DELAY_SECONDS);
                    if (autoRunDelaySeconds == null) {
                        nextAutoRunTime = new Date();
                    } else {
                        nextAutoRunTime = new Date(System.currentTimeMillis() + autoRunDelaySeconds * 1000);
                    }
                }
                instance.setNextAutoRunTime(nextAutoRunTime);
            }

            context.setNewInstance(true);
            Map<String, Object> extDataMap = new HashMap<>();
            Map<String, String> clientInfo = context.getClientInfo();
            if (clientInfo != null) {
                extDataMap.put(ProcessUnitConstants.INSTANCE_EXT_DATA_CREATE_CLIENT_INFO, clientInfo);
            }
            Map<String, String> variables = context.getParam().getVariables();
            if (variables != null) {
                instance.setVars(JSON.toJSONString(variables));
            }
            if (extDataMap.size() > 0) {
                instance.setExtData(JSON.toJSONString(extDataMap));
            }
            processUnitRepository.saveInstance(instance);
            if (log.isDebugEnabled()) {
                log.debug("New process unit instance created:{}", instance);
            } else if (log.isInfoEnabled()) {
                log.info("New process unit instance created, instance no:" + instance.getInstanceNo());
            }
            context.setNewInstance(true);
        }
        context.setInstance(instance);
    }
    
    public void executeMessage(String instanceNo) {
        executeMessage(instanceNo, null, null);
    }

    public void executeMessage(String instanceNo, String unitCode, String bizNo) {
        ProcessUnitInstanceEntity instance = processUnitRepository.getInstance(instanceNo, unitCode, bizNo);
        if (instance == null) {
            log.warn("Process unit instance " + instanceNo + " not exists");
            return;
        }
        ExecParam param = new ExecParam();
        param.setInstanceNo(instanceNo);
        param.setBizNo(instance.getBizNo());
        param.setUnitCode(instance.getProcessUnitCode());
        param.setExecType(ProcessUnitConstants.EXEC_TYPE_MSG_ASYNC);
        try {
            asyncServerProcessUnitExecutor.execute(param);
        } catch (Exception e) {
            AlertUtil.doAlert("processUnit-executeMessage", "Process unit execute exception,", e, instance, instance.getProductCode());
            throw e;
        }
    }

    public SyncBeforeCallRes syncBeforeCall(SyncBeforeCallReq req) {
        ExecParam param = ProcessUnitExecuteConverter.INSTANCE.convert(req);
        param.setExecType(ProcessUnitConstants.EXEC_TYPE_SYNC);
        ExecContext context = new ExecContext(param);
        context.setUnitCode(param.getUnitCode());
        context.setBizNo(param.getBizNo());
        context.setClientInfo(param.getClientInfo());
        ExecPolicy policy = syncServerProcessUnitExecutor.beforeCall(context);
        SyncBeforeCallRes res = new SyncBeforeCallRes();
        res.setExecPolicy(policy);
        Map<String, Object> configMap = context.getProcessUnit().getConfigMap();
        if (configMap != null) {
            Map<String, Object> clientConfigMap = new HashMap<>(1);
            clientConfigMap.put(ProcessUnitConstants.CONF_CATCH_EXCEPTION,
                    configMap.get(ProcessUnitConstants.CONF_CATCH_EXCEPTION));
            res.setUnitConf(context.getProcessUnit().getConfigMap());
        }
        res.setExecutionNo(context.getExecutionNo());
        res.setInstanceNo(context.getInstanceNo());
        res.putData(ContextDataKeys.EXECUTION_EXT_DATA_LOCK_REQUEST_ID, context.getLockRequestId());
        res.putData(ContextDataKeys.EXECUTION_START_TIME, System.currentTimeMillis() + "");
        res.putData(ContextDataKeys.PROCESS_UNIT_INVALID,
                (context.getProcessUnit().getStatus() == ProcessUnitConstants.STATUS_INVALID) + "");
        if (context.getInstance() != null && context.getInstance().getVars() != null) {
            Map<String, String> varsMap = JSON.parseObject(context.getInstance().getVars(), Map.class);
            res.setVariables(varsMap);
        }
        if (context.getInstance() != null && context.getInstance().getExtData() != null) {
            Map<String, Object> extDataMap = JSON.parseObject(context.getInstance().getExtData(), Map.class);
            res.putData(ContextDataKeys.INSTANCE_EXT_DATA, context.getInstance().getExtData());
        }
        return res;
    }

    public SyncAfterCallRes syncAfterCall(SyncAfterCallReq req) {
        if (req.getContextData() != null
                && "true".equals(req.getContextData().get(ContextDataKeys.PROCESS_UNIT_INVALID))) {
            log.info("process unit invalid.");
            return new SyncAfterCallRes();
        }
        ExecContext context = buildAfterCallContext(req);
        syncServerProcessUnitExecutor.afterCall(context);
        return new SyncAfterCallRes();
    }

    public ExecuteRes execute(ExecuteReq req) {
        log.info("Process unit execute start:{}", req);
        ExecParam param = new ExecParam();
        param.setInstanceNo(req.getUnitInstanceNo());
        param.setUnitCode(req.getUnitCode());
        param.setBizNo(req.getBizNo());
        param.setExecType(ProcessUnitConstants.EXEC_TYPE_MANUAL_ASYNC);
        ExecResult result = asyncServerProcessUnitExecutor.execute(param);
        ExecuteRes res = new ExecuteRes(result.getResult(), result.getResponseContent());
        log.info("Process unit execute end:{}", res);
        return res;
    }

    public BatchExecuteRes batchExecute(BatchExecuteReq req) {
        log.info("Batch execute req:{}", req);
        if (BatchExecuteReq.TYPE_BATCH.equals(req.getType())) {
            List<ProcessUnitInstanceKey> keyList = new ArrayList<ProcessUnitInstanceKey>();
            ProcessUnitEntity unit = null;
            if (req.getInstanceNoList() != null) {
                String unitCode = null;
                if (req.getProcessUnitCodeList() != null) {
                    if (req.getProcessUnitCodeList().size() > 1) {
                        throw new EasyFlowException("processUnitCodeList more than 1");
                    } else {
                        unitCode = req.getProcessUnitCodeList().get(0);
                        unit = processUnitRepository.getProcessUnitByCode(unitCode);
                    }
                }
                String finalUnitCode = unitCode;
                req.getInstanceNoList().forEach(instanceNo -> keyList.add(new ProcessUnitInstanceKey(instanceNo, finalUnitCode, null)));
            } else if (req.getBizNoList() != null) {
                if (req.getProcessUnitCodeList() == null || req.getProcessUnitCodeList().size() != 1) {
                    throw new EasyFlowException("processUnitCodeList should be only 1");
                }
                req.getBizNoList().forEach(bizNo -> keyList.add(new ProcessUnitInstanceKey(req.getProcessUnitCodeList().get(0), bizNo)));
            } else {
                throw new EasyFlowException("Param illegal:" + req);
            }
            runOneBatch(unit, keyList, req.getRequestContext(), req);
        } else {
            List<ProcessUnitEntity> unitList = null;
            if (req.getProcessUnitCodeList() != null && !req.getProcessUnitCodeList().isEmpty()) {
                unitList = new ArrayList<>();
                for (String code : req.getProcessUnitCodeList()) {
                    unitList.add(processUnitRepository.getProcessUnitByCode(code));
                }
            } else {
                unitList = processUnitRepository.findAllProcessUnitList();
            }
            for (ProcessUnitEntity unit : unitList) {
                if (ProcessUnitConstants.STATUS_INVALID.equals(unit.getStatus())
                        || Boolean.FALSE.equals(unit.getConfig(ProcessUnitConstants.CONF_AUTO_ASYNC_RUN))) {
                    continue;
                }
                if (!processUnitCodeMatch(unit.getProcessUnitCode(), req.getProcessUnitCodeList(),
                        req.getExcludeProcessUnitCodeList())) {
                    continue;
                }
                log.info("Start process unit:" + unit.getProcessUnitCode());
                QueryAsyncInstanceVO query = new QueryAsyncInstanceVO();
                query.setMaxCount(asyncMaxCount);
                query.setUnitCode(unit.getProcessUnitCode());

                Date currentDate = new Date();
                if (req.getNextAutoRunTimeEnd() != null) {
                    query.setNextAutoRunTime(req.getNextAutoRunTimeEnd());
                } else {
                    query.setNextAutoRunTime(currentDate);
                }
                if (req.getNextAutoRunTimeStart() != null) {
                    query.setNextAutoRunTimeStart(req.getNextAutoRunTimeStart());
                } else if (req.getNextAutoRunTimeMaxInterval() != null) {
                    
                    Date nextAutoRunTimeStart = new Date(currentDate.getTime() - req.getNextAutoRunTimeMaxInterval() * 1000);
                    query.setNextAutoRunTimeStart(nextAutoRunTimeStart);
                }
                if (req.getProductCodeList() != null && ! req.getProductCodeList().isEmpty()) {
                    query.setProductCodeList(req.getProductCodeList());
                }
                if (req.getExcludeProductCodeList() != null && ! req.getExcludeProductCodeList().isEmpty()) {
                    query.setExcludeProductCodeList(req.getExcludeProductCodeList());
                }
                if (req.getResultList() != null && ! req.getResultList().isEmpty()) {
                    query.setResultList(req.getResultList());
                }
                query.setVariableList(ProcessUnitExecuteConverter.INSTANCE.convertVariableList(req.getVariableList()));
                List<ProcessUnitInstanceKey> instanceKeyList = processUnitRepository.findAsyncInstanceKeyList(query);
                runOneBatch(unit, instanceKeyList, req.getRequestContext(), req);

            }
        }
        BatchExecuteRes res = new BatchExecuteRes();
        return res;
    }

    private boolean processUnitCodeMatch(String processUnitCode, List<String> processUnitCodeList,
            List<String> excludeProcessUnitCodeList) {
        if (processUnitCodeList != null && !processUnitCodeList.contains(processUnitCode)) {
            return false;
        }
        if (excludeProcessUnitCodeList != null && excludeProcessUnitCodeList.contains(processUnitCode)) {
            return false;
        }
        return true;
    }

    private void runOneBatch(ProcessUnitEntity entity, List<ProcessUnitInstanceKey> instanceKeyList,
            Map<String, Object> requestContext, BatchExecuteReq req) {
        log.info("Execute process unit instance list:{}", instanceKeyList);
        if (instanceKeyList.size() == 0) {
            return;
        }
        String batchRunPolicy = req.getBatchRunPolicy();
        if (batchRunPolicy == null && entity != null) {
            batchRunPolicy = (String) entity.getConfig(ProcessUnitConstants.BATCH_RUN_POLICY);
        }
        if (ProcessUnitConstants.BATCH_RUN_POLICY_MULTIPLE_THREAD.equals(batchRunPolicy)) {
            Integer threadNum = req.getThreadNum();
            if (threadNum == null) {
            Map<String, Object> policyConf = (Map<String, Object>) entity
                    .getConfig(ProcessUnitConstants.BATCH_RUN_POLICY_CONF);
                threadNum = policyConf == null ? null : (Integer) policyConf.get("threadNum");
            }
            if (threadNum == null) {
                throw new IllegalArgumentException("Thread num is null, unitCode:" + entity.getProcessUnitCode());
            }
            runOneBatchWithMultipleThread(entity, instanceKeyList, requestContext, threadNum);
        } else {
            runOneBatchWithSingleThread(instanceKeyList, requestContext);
        }
    }

    private void runOneBatchWithSingleThread(List<ProcessUnitInstanceKey> instanceKeyList, Map<String, Object> requestContext) {
        for (ProcessUnitInstanceKey key : instanceKeyList) {
            if (stop) {
                log.warn("Process unit shutdown, instanceNo:{}", key.getInstanceNo());
                throw new UserException(ProcessUnitErrorCodeEnum.PU_0101.name(), ProcessUnitErrorCodeEnum.PU_0101.getDesc());
            }            
            runOneInstance(key.getInstanceNo(), key.getUnitCode(), key.getBizNo(), requestContext);
        }
    }

    private void runOneBatchWithMultipleThread(ProcessUnitEntity entity, List<ProcessUnitInstanceKey> instanceKeyList,
            Map<String, Object> requestContext, int threadNum) {
        if (threadNum > maxThreadNumPerUnitCode) {
            log.warn("Thread num:{} is greater than max num {}, default to max num", threadNum, maxThreadNumPerUnitCode);
            threadNum = maxThreadNumPerUnitCode;
        }
        CountDownLatch countDownLatch = new CountDownLatch(instanceKeyList.size());
        AtomicInteger count = new AtomicInteger(-1);
        for (int i = 0; i < threadNum; i++) {
            createThread(() -> {
                log.info("Thread start " + entity.getProcessUnitCode() + " batch process");
                while (true) {
                    int index = count.addAndGet(1);
                    if (index < instanceKeyList.size()) {
                        ProcessUnitInstanceKey key = instanceKeyList.get(index);
                        try {
                            if (stop) {
                                log.warn("Process unit server shutdown, not execute instance:{}", key.getInstanceNo());
                                continue;
                            }
                            runOneInstance(key.getInstanceNo(), key.getUnitCode(), key.getBizNo(), requestContext);
                        } catch (Throwable t) {
                            log.error("Process unit instance:" + key.getInstanceNo() + " execute exception:" + t.getMessage(), t);
                        } finally {
                            countDownLatch.countDown();
                        }
                    } else {
                        break;
                    }
                }
                log.info("Thread end " + entity.getProcessUnitCode() + " batch process");
            }, "processunit-" + entity.getProcessUnitCode() + "-" + i).start();
        }
        try {
            countDownLatch.await();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        if (stop) {
            log.warn("Process unit server shutdown, stop exeucte");
            throw new UserException(ProcessUnitErrorCodeEnum.PU_0101.name(), ProcessUnitErrorCodeEnum.PU_0101.getDesc());
        }
    }
    
    protected Thread createThread(Runnable target, String name) {
        return new Thread(target, name);
    }

    private void runOneInstance(String instanceNo, String unitCode, String bizNo, Map<String, Object> requestContext) {
        log.info("Start execute instance:{} unitCode:{} bizNo:{}", instanceNo, unitCode, bizNo);
        ExecParam param = new ExecParam();
        param.setInstanceNo(instanceNo);
        param.setExecType(ProcessUnitConstants.EXEC_TYPE_TASK_ASYNC);
        param.setRequestContext(requestContext);
        ProcessUnitInstanceEntity instance = processUnitRepository.getInstance(instanceNo, unitCode, bizNo);
        if (instance == null) {
            throw new UserException("Process unit instance:" + instanceNo + " " + unitCode + " " + bizNo + " not exists");
        }
        param.setBizNo(instance.getBizNo());
        param.setUnitCode(instance.getProcessUnitCode());
        try {
            asyncServerProcessUnitExecutor.execute(param);
            log.info("End execute instance:{} unitCode:{} bizNo:{}", instanceNo, unitCode, bizNo);
        } catch (Exception e) {
            log.error("Process unit instance execute exception:{} unitCode:{} bizNo:{} message:{}", instanceNo, unitCode, bizNo, e.getMessage(), e);
            AlertUtil.doAlert("processUnit-runOneBatch", "Process unit instance execute exception,", e, param, instance.getProductCode());
        }
    }

    private ExecContext buildAfterCallContext(SyncAfterCallReq req) {
        String unitCode = req.getUnitCode();
        String executionNo = req.getExecutionNo();
        String instanceNo = req.getInstanceNo();
        String bizNo = req.getBizNo();
        ExecParam param = new ExecParam();
        param.setExecType(ProcessUnitConstants.EXEC_TYPE_SYNC);
        ExecContext context = new ExecContext(param);
        ProcessUnitEntity processUnit = processUnitRepository.getProcessUnitByCode(unitCode);
        context.setBizNo(bizNo);
        context.setUnitCode(unitCode);
        context.setInstanceNo(instanceNo);
        context.setExecutionNo(executionNo);
        context.setProcessUnit(processUnit);
        context.getResult().setResult(req.getResult());
        context.getResult().setResponseContent(req.getResponseContent());
        context.setLockRequestId(req.getData(ContextDataKeys.EXECUTION_EXT_DATA_LOCK_REQUEST_ID));
        String requestTimeStr = req.getData(ContextDataKeys.EXECUTION_START_TIME);
        if (requestTimeStr != null && ! requestTimeStr.isEmpty()) {
            context.setRequestTime(Long.parseLong(requestTimeStr));
        }
        context.setVariables(req.getVariables());
        context.getResult().setAutoRunFlag(req.getAutoRunFlag());
        context.getResult().setNextAutoRunTime(req.getNextAutoRunTime());
        String extDataStr = req.getData(ContextDataKeys.INSTANCE_EXT_DATA);
        context.setExtDataStr(extDataStr);        
        return context;
    }


    public void updateInstance(ProcessUnitInstanceDTO dto) {
        ProcessUnitInstanceEntity entity = null;
        if (dto.getInstanceNo() != null) {
            entity = processUnitRepository.getInstance(dto.getInstanceNo(), dto.getProcessUnitCode(), dto.getBizNo());
        } else if (dto.getProcessUnitCode() != null && dto.getBizNo() != null) {
            entity = processUnitRepository.getInstanceByUnitCodeAndBizNo(dto.getProcessUnitCode(), dto.getBizNo());
        }
        if (entity == null) {
            throw new UserException("process unit instance is null");
        }
        if (dto.getAutoRunFlag() != null) {
            entity.setAutoRunFlag(dto.getAutoRunFlag());
        }
        if (dto.getNextAutoRunTime() != null) {
            entity.setNextAutoRunTime(dto.getNextAutoRunTime());
        }
        if (dto.getAutoRunTimes() != null) {
            entity.setAutoRunTimes(dto.getAutoRunTimes());
        }
        if (dto.getRequestContent() != null) {
            entity.setRequestContent(dto.getRequestContent());
        }
        if (dto.getResponseContent() != null) {
            entity.setResponseContent(dto.getResponseContent());
        }
        if (dto.getResult() != null) {
            entity.setResult(dto.getResult());
        }
        processUnitRepository.updateInstance(entity);
    }
    
    public PagerResult pagerQueryProcessUnitInstance(PagerCondition pagerQueryReq) {
        FieldEntry unitCodeField = pagerQueryReq.getField("unitCode");
        String unitCode = unitCodeField == null ? null : (String) unitCodeField.getValue();
        if (unitCode != null && ! unitCode.isEmpty()) {
            ProcessUnitEntity processUnit = processUnitRepository.getProcessUnitByCode(unitCode);
            if (processUnit == null) {
                throw new UserException("Process unit " + unitCode + " not exists");
            }
        }
        FieldEntry queryTypeField = pagerQueryReq.getField("queryType");
        String queryType = queryTypeField.stringValue();
        int pageSize = pagerQueryReq.getPageSize();
        if (pageSize > pagerQueryMaxPageSize) {
            throw new UserException(ExportResponseCode.INVALID.getCode(), " page record num should less than " + pagerQueryMaxPageSize);
        }
        if (ProcessUnitConstants.INSTANCE_QUERY_TYPE_SINGLE.equals(queryType)) {
            
        } else if (ProcessUnitConstants.INSTANCE_QUERY_TYPE_CREATED_DATE_RANGE.equals(queryType)) {
            FieldEntry createdDateStartField = pagerQueryReq.getField("createdDateStart");
            FieldEntry createdDateEndField = pagerQueryReq.getField("createdDateEnd");
            String createdDateStartStr = createdDateStartField == null ? null : (String) createdDateStartField.getValue();
            String createdDateEndStr = createdDateEndField == null ? null : (String) createdDateEndField.getValue();
            Date createdDateStart = null;
            try {
                createdDateStart = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(createdDateStartStr);
            } catch (Exception e) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "date " + createdDateStartStr + " illegal");
            }
            Date createdDateEnd = null;
            try { 
                createdDateEnd = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(createdDateEndStr);
            } catch (Exception e) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "date " + createdDateEndStr + " illegal");
            }
            if (createdDateEnd.getTime() - createdDateStart.getTime() > pagerQueryTimeRangeDays * 24L * 3600 * 1000) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "date range greater than" + pagerQueryTimeRangeDays + " days");
            }
            
        }
        return processUnitRepository.pagerQueryProcessUnitInstance(pagerQueryReq);
    }
    
    public PagerResult pagerQueryProcessUnitExecution(PagerCondition pagerQueryReq) {
        FieldEntry unitCodeField = pagerQueryReq.getField("unitCode");
        String unitCode = unitCodeField == null ? null : (String) unitCodeField.getValue();
        if (unitCode != null && !unitCode.isEmpty()) {
            ProcessUnitEntity processUnit = processUnitRepository.getProcessUnitByCode(unitCode);
            if (processUnit == null) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "process unit " + unitCode + " not exists");
            }
        }
        int pageSize = pagerQueryReq.getPageSize();
        if (pageSize > pagerQueryMaxPageSize) {
            throw new UserException(ExportResponseCode.INVALID.getCode(), "page record num should not greater than " + pagerQueryMaxPageSize);
        }
        FieldEntry queryTypeField = pagerQueryReq.getField("queryType");
        String queryType = queryTypeField.stringValue();
        if (ProcessUnitConstants.EXECUTION_QUERY_TYPE_INSTANCE.equals(queryType)) {
            
        } else if (ProcessUnitConstants.EXECUTION_QUERY_TYPE_REQUEST_TIME_RANGE.equals(queryType)) {
            FieldEntry requestTimeStartField = pagerQueryReq.getField("requestTimeStart");
            FieldEntry requestTimeEndField = pagerQueryReq.getField("requestTimeEnd");
            String requestTimeStartStr = requestTimeStartField == null ? null : (String) requestTimeStartField.getValue();
            String requestTimeEndStr = requestTimeEndField == null ? null : (String) requestTimeEndField.getValue();
            Date requestTimeStart = null;
            try {
                requestTimeStart = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(requestTimeStartStr);
            } catch (Exception e) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "date " + requestTimeStartStr + " illegal");
            }
            Date requestTimeEnd = null;
            try { 
                requestTimeEnd = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(requestTimeEndStr);
            } catch (Exception e) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "date " + requestTimeEndStr + " illegal");
            }
            if (requestTimeEnd.getTime() - requestTimeStart.getTime() > pagerQueryTimeRangeDays * 24L * 3600 * 1000) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "date range should not greater than " + pagerQueryTimeRangeDays + " days");
            }
            
        }       
        
        return processUnitRepository.pagerQueryProcessUnitExecution(pagerQueryReq);
    }
    
    public BatchExecuteRes updateAutoRunFlag(BatchExecuteReq req) {
        log.info("Update auto run flag ,request:{}", req);
        if (BatchExecuteReq.TYPE_BATCH.equals(req.getType())) {
            List<ProcessUnitInstanceKey> keyList = new ArrayList<ProcessUnitInstanceKey>();
            ProcessUnitEntity unit = null;
            if (req.getInstanceNoList() != null) {
                String unitCode = null;
                if (req.getProcessUnitCodeList() != null) {
                    if (req.getProcessUnitCodeList().size() > 1) {
                        throw new UserException("processUnitCodeList at most has one");
                    } else {
                        unitCode = req.getProcessUnitCodeList().get(0);
                        unit = processUnitRepository.getProcessUnitByCode(unitCode);
                    }
                }
                String finalUnitCode = unitCode;
                req.getInstanceNoList().forEach(instanceNo -> keyList.add(new ProcessUnitInstanceKey(instanceNo, finalUnitCode, null)));
            } else if (req.getBizNoList() != null) {
                if (req.getProcessUnitCodeList() == null || req.getProcessUnitCodeList().size() != 1) {
                    throw new UserException("processUnitCodeList should only one");
                }
                req.getBizNoList().forEach(bizNo -> keyList.add(new ProcessUnitInstanceKey(req.getProcessUnitCodeList().get(0), bizNo)));
            } else {
                throw new UserException("Param illegal:" + req);
            }
            updateOneBatchAutoRunFlag(unit, keyList, req);
        } else {
            if (req.getNextAutoRunTimeEnd() == null) {
                throw new UserException("nextAutoRunTimeEnd is null");
            }
            List<ProcessUnitEntity> unitList = null;
            if (req.getProcessUnitCodeList() != null && !req.getProcessUnitCodeList().isEmpty()) {
                unitList = new ArrayList<>();
                for (String code : req.getProcessUnitCodeList()) {
                    unitList.add(processUnitRepository.getProcessUnitByCode(code));
                }
            } else {
                unitList = processUnitRepository.findAllProcessUnitList();
            }
            for (ProcessUnitEntity unit : unitList) {
                log.info("Start process unit:" + unit.getProcessUnitCode());
                QueryAsyncInstanceVO query = new QueryAsyncInstanceVO();
                query.setMaxCount(asyncMaxCount);
                query.setUnitCode(unit.getProcessUnitCode());

                query.setNextAutoRunTimeStart(req.getNextAutoRunTimeStart());
                query.setNextAutoRunTime(req.getNextAutoRunTimeEnd());
                List<ProcessUnitInstanceKey> instanceKeyList = processUnitRepository.findAsyncInstanceKeyList(query);
                updateOneBatchAutoRunFlag(unit, instanceKeyList, req);

            }
        }
        return new BatchExecuteRes();
    }
    
    private void updateOneBatchAutoRunFlag(ProcessUnitEntity entity, List<ProcessUnitInstanceKey> instanceKeyList,
            BatchExecuteReq req) {
        log.info("Update autoRunFlag list:{}", instanceKeyList);
        if (instanceKeyList.size() == 0) {
            return;
        }
        for (ProcessUnitInstanceKey key : instanceKeyList) {
            if (stop) {
                log.warn("Process unit server shutdown, stop update AutoRunFlag:{}", key.getInstanceNo());
                throw new UserException(ProcessUnitErrorCodeEnum.PU_0101.name(),
                        ProcessUnitErrorCodeEnum.PU_0101.getDesc());
            }
            ProcessUnitInstanceEntity instance = processUnitRepository.getInstance(key.getInstanceNo(),
                    key.getUnitCode(), key.getBizNo());
            if (instance == null) {
                throw new UserException("instance:" + key.getInstanceNo() + " " + key.getUnitCode() + " " + key.getBizNo() + " does not exist");
            }    
            if (entity == null) {
                entity = processUnitRepository.getProcessUnitByCode(instance.getProcessUnitCode());
            }
            ProcessUnitEntity finalEntity = entity;            
            lockManager.doInlock(instance.getProcessUnitCode(), instance.getBizNo(), null, () -> {
                        puTransactionTemplate.doInTransaction(finalEntity, instance.getBizNo(), null, null, () -> {
                            ProcessUnitInstanceEntity unitInstance = processUnitRepository.getInstance(instance.getInstanceNo(),
                                    instance.getProcessUnitCode(), instance.getBizNo());
                            if (! Boolean.TRUE.equals(unitInstance.getAutoRunFlag())) {
                                log.warn("instance:" + unitInstance.getInstanceNo() + " " + unitInstance.getProcessUnitCode() + " " + unitInstance.getBizNo() + " autoRun is not true, skipped");
                                return null;
                            }
                            unitInstance.setAutoRunFlag(false);
                            String extData = unitInstance.getExtData();
                            Map<String, Object> extDataMap = JSON.parseObject(extData, Map.class);
                            if (extDataMap == null) {
                                extDataMap = new HashMap<String, Object>();
                            }
                            extDataMap.put(ProcessUnitConstants.INSTANCE_EXT_DATA_UPDATE_AUTO_RUN_TIME, System.currentTimeMillis());
                            unitInstance.setExtData(JSON.toJSONString(extDataMap));
                            log.warn("instance:" + unitInstance.getInstanceNo() + " " + unitInstance.getProcessUnitCode() + " " + unitInstance.getBizNo() + " autoRun is updated to false");
                            processUnitRepository.updateInstance(unitInstance);
                            return null;
                        });
                        return null;
                    });
        }

    }    
    
    public void shutdown() {
        stop = true;
    }
    
    public String getExecuteMessageTopic() {
        return executeMessageTopic;
    }

    public void setExecuteMessageTopic(String executeMessageTopic) {
        this.executeMessageTopic = executeMessageTopic;
    }

    public AsyncServerProcessUnitExecutor getAsyncServerProcessUnitExecutor() {
        return asyncServerProcessUnitExecutor;
    }

    public void setAsyncServerProcessUnitExecutor(AsyncServerProcessUnitExecutor asyncServerProcessUnitExecutor) {
        this.asyncServerProcessUnitExecutor = asyncServerProcessUnitExecutor;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }

    public MessageSendService getMessageSendService() {
        return messageSendService;
    }

    public void setMessageSendService(MessageSendService messageSendService) {
        this.messageSendService = messageSendService;
    }

    public String getCreateMessageTopic() {
        return createMessageTopic;
    }

    public void setCreateMessageTopic(String createMessageTopic) {
        this.createMessageTopic = createMessageTopic;
    }

    public String getUpdateMessageTopic() {
        return updateMessageTopic;
    }

    public void setUpdateMessageTopic(String updateMessageTopic) {
        this.updateMessageTopic = updateMessageTopic;
    }

    public int getAsyncMaxCount() {
        return asyncMaxCount;
    }

    public void setAsyncMaxCount(int asyncMaxCount) {
        this.asyncMaxCount = asyncMaxCount;
    }

    public int getPagerQueryTimeRangeDays() {
        return pagerQueryTimeRangeDays;
    }

    public void setPagerQueryTimeRangeDays(int pagerQueryTimeRangeDays) {
        this.pagerQueryTimeRangeDays = pagerQueryTimeRangeDays;
    }

    public int getPagerQueryMaxPageSize() {
        return pagerQueryMaxPageSize;
    }

    public void setPagerQueryMaxPageSize(int pagerQueryMaxPageSize) {
        this.pagerQueryMaxPageSize = pagerQueryMaxPageSize;
    }

    public int getMaxThreadNumPerUnitCode() {
        return maxThreadNumPerUnitCode;
    }

    public void setMaxThreadNumPerUnitCode(int maxThreadNumPerUnitCode) {
        this.maxThreadNumPerUnitCode = maxThreadNumPerUnitCode;
    }

    public SyncServerProcessUnitExecutor getSyncServerProcessUnitExecutor() {
        return syncServerProcessUnitExecutor;
    }

    public void setSyncServerProcessUnitExecutor(SyncServerProcessUnitExecutor syncServerProcessUnitExecutor) {
        this.syncServerProcessUnitExecutor = syncServerProcessUnitExecutor;
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
