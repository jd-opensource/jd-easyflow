package com.jd.easyflow.process.domain.service;

import static com.jd.easyflow.process.adapter.export.constant.ProcessInstanceConstants.STATUS_CANCELED;
import static com.jd.easyflow.process.adapter.export.constant.ProcessInstanceConstants.STATUS_CLOSE;
import static com.jd.easyflow.process.domain.constant.ProcessTaskConstants.TASK_STATUS_CANCELED;
import static com.jd.easyflow.process.domain.constant.ProcessTaskConstants.TASK_STATUS_PENDING;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.ObjectUtils;

import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.common.util.MessageUtil;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.lock.Locker;
import com.jd.easyflow.message.MessageSendService;
import com.jd.easyflow.process.adapter.export.constant.ProcessInstanceConstants;
import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.CanCancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CanCancelProcessInstanceRes;
import com.jd.easyflow.process.adapter.export.dto.instance.CancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeExecutionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.StdProcessContextDTO;
import com.jd.easyflow.process.adapter.message.ProcessInstanceStatusMessage;
import com.jd.easyflow.process.adapter.message.ProcessNodeInstanceStatusMessage;
import com.jd.easyflow.process.adapter.message.ProcessTaskStatusMessage;
import com.jd.easyflow.process.domain.constant.ProcessConstants;
import com.jd.easyflow.process.domain.constant.ProcessTaskConstants;
import com.jd.easyflow.process.domain.constant.StdProcessConstants;
import com.jd.easyflow.process.domain.converter.ProcessInstanceDomainConverter;
import com.jd.easyflow.process.domain.converter.ProcessTaskDomainConverter;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeExecutionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEventEntity;
import com.jd.easyflow.process.domain.model.vo.CreateProcessInstanceReqVO;
import com.jd.easyflow.process.domain.model.vo.CreateProcessInstanceResVO;
import com.jd.easyflow.process.domain.model.vo.ProcessDefinitionForListVO;
import com.jd.easyflow.process.domain.model.vo.QueryProcessNodeReq;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessResVO;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.process.domain.repository.ProcessTaskRepository;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class ProcessInstanceDomainService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessInstanceDomainService.class);


    private static final int MAX_PAGE_SIZE = 10000;

    @Autowired
    private ProcessRepository processRepository;
    private Locker locker;
    private TransactionTemplate transactionTemplate;

    @Value(ProcessConstants.TOPIC_PROCESS_INSTANCE_STATUS)
    private String processInstanceStatusTopic;
    
    @Value(ProcessConstants.TOPIC_NODE_INSTANCE_STATUS)
    private String nodeInstanceStatusTopic;

    @Value(ProcessTaskConstants.TOPIC_TASK_STATUS)
    private String taskStatusTopic;

    @Value(ProcessConstants.CREATED_DATE_POLICY)
    private String createdDatePolicy;

    private MessageSendService messageSendService;

    @Autowired
    private ProcessScheduleDomainService processScheduleDomainService;

    @Autowired
    private ProcessDefinitionDomainService processDefinitionDomainService;

    @Autowired
    private ProcessTaskRepository processTaskRepository;

    public static final String CACHE_KEY_PREFIX = "PROCESS_DEF_VERSION_";

    private static final Integer INIT_VERSION = 0;

    public static final String PROCESS_DEFINITION_LOCK = "_MODIFY_PROCESS_DEFINITION_";

    public static final String EXECUTION_PERSIST_POLICY_SYNC = "SYNC";
    public static final String EXECUTION_PERSIST_POLICY_ASYNC = "ASYNC";

    private String executionPersistPolicy = EXECUTION_PERSIST_POLICY_ASYNC;
    
    public static final String EXECUTION_PERSIST_TYPE_ALL = "ALL";
    public static final String EXECUTION_PERSIST_TYPE_NEXT = "NEXT";
    public static final String EXECUTION_PERSIST_TYPE_EVENT_AND_NEXT = "EVENT_AND_NEXT";
    public static final String EXECUTION_PERSIST_TYPE_NONE = "NONE";
    
    private String executionPeristType = EXECUTION_PERSIST_TYPE_ALL;

    public PagerResult<ProcessDefinitionForListVO> pageQueryProcessDefinition(PagerCondition pagerQueryReq) {
        return processRepository.pageQueryProcessDefinition(pagerQueryReq);
    }

    public PagerResult<ProcessInstanceEntity> pageQueryProcessInstance(PagerCondition pagerQueryReq) {
        if (pagerQueryReq.getPageSize() > MAX_PAGE_SIZE) {
            throw new UserException("The max page size is " + MAX_PAGE_SIZE);
        }
            return processRepository.pageQueryProcessInstance(pagerQueryReq);
    }

    public ProcessInstanceEntity queryProcessInstance(String instanceNo) {
            return processRepository.getByProcessInstanceNo(instanceNo);
    }

    public List<ProcessNodeInstanceEntity> queryProcessInstanceNode(String instanceNo) {
        QueryProcessNodeReq queryProcessNodeReq = QueryProcessNodeReq.builder().processInstanceNo(instanceNo).build();
        List<ProcessNodeInstanceEntity> nodeInstanceEntities = processRepository.findNodeInstances(queryProcessNodeReq);
        return nodeInstanceEntities;
    }

    public CreateProcessInstanceResVO createProcessInstance(CreateProcessInstanceReqVO vo) {
        ScheduleProcessReqVO req = new ScheduleProcessReqVO();
        req.setProcessId(vo.getProcessId());
        req.setNodeIds(vo.getNodeIds());
        req.setParam(vo.getParam());
        Map<String, Object> dataMap = vo.getDataMap();
        if (dataMap == null) {
            dataMap = new HashMap<>();
        }
        req.setDataMap(dataMap);
        if (vo.getProcessType() != null) {
            dataMap.put(StdProcessConstants.PARAM_PROCESS_TYPE, vo.getProcessType());
        }
        if (vo.getProductCode() != null) {
            dataMap.put(StdProcessConstants.PARAM_PRODUCT_CODE, vo.getProductCode());
        }
        if (vo.getCreator() != null) {
            dataMap.put(StdProcessConstants.PARAM_USER, vo.getCreator());
        }
        if (vo.getBizNo() != null) {
            dataMap.put(StdProcessConstants.PARAM_BIZNO, vo.getBizNo());
        }
        if (vo.getInstanceName() != null) {
            dataMap.put(StdProcessConstants.PARAM_INSTANCE_NAME, vo.getInstanceName());
        }
        if (vo.getBizStatus() != null) {
            dataMap.put(StdProcessConstants.PARAM_BIZ_STATUS, vo.getBizStatus());
        }
        if (vo.getBizData() != null) {
            dataMap.put(StdProcessConstants.PARAM_BIZ_DATA, vo.getBizData());
        }
        if (vo.getKeyField() != null) {
            dataMap.put(StdProcessConstants.PARAM_KEY_FIELD, vo.getKeyField());
        }
        if (vo.getKeyField2() != null) {
            dataMap.put(StdProcessConstants.PARAM_KEY_FIELD2, vo.getKeyField2());
        }
        dataMap.put(StdProcessConstants.PARAM_OP_TYPE, StdProcessConstants.OP_TYPE_CREATE);
        if (!dataMap.containsKey(FlowConstants.PARAM_DATA_EVENT)) {
            dataMap.put(FlowConstants.PARAM_DATA_EVENT, "PROCESS_INSTANCE_CREATE");
        }
        ScheduleProcessResVO res = processScheduleDomainService.schedule(req);
        CreateProcessInstanceResVO createRes = new CreateProcessInstanceResVO();
        createRes.setProcessInstanceNo(res.getProcessInstanceNo());
        return createRes;
    }

    public void updateProcessObject(Integer persistOp, Object o, StdProcessContextDTO process,
            List<Map<String, Object>> actionList) {
        
        boolean sendNodeInstanceStatusMessage = false;
        String nodeInstanceStatusTopic = null;
        if (process.getProcessProperties() != null
                && process.getProcessProperties().get("nodeInstanceStatusMessage") != null) {
            Map<String, Object> messageConfig = (Map<String, Object>) process.getProcessProperties()
                    .get("nodeInstanceStatusMessage");
            sendNodeInstanceStatusMessage = Boolean.TRUE.equals(messageConfig.get("enable"));
            if (sendNodeInstanceStatusMessage) {
                nodeInstanceStatusTopic = (String) messageConfig.getOrDefault("topic", this.nodeInstanceStatusTopic);
            }
        }
        String engine = (process.getProcess() == null || process.getProcess().getExtProperties() == null) ? null
                : (String) process.getProcess().getExtProperties().get("engine");
        
        if (o instanceof ProcessInstanceDTO) {
            ProcessInstanceEntity entity = ProcessInstanceDomainConverter.INSTANCE.convert((ProcessInstanceDTO) o);
            if (persistOp == null) {
                ProcessInstanceEntity currentEntity = processRepository.getByProcessInstanceNo(entity.getInstanceNo());
                persistOp = currentEntity == null ? ProcessTransactionConstants.PERSIST_OP_ADD
                        : ProcessTransactionConstants.PERSIST_OP_UPDATE;
            }
            if (persistOp == ProcessTransactionConstants.PERSIST_OP_ADD) {
                if (log.isDebugEnabled()) {
                    log.debug("Add " + entity.getInstanceNo());
                }
                switch (createdDatePolicy) {
                case ProcessConstants.CREATED_DATE_POLICY_CLIENT: {
                    processRepository.saveProcessInstanceWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_SERVER: {
                    entity.setCreatedDate(new Date());
                    processRepository.saveProcessInstanceWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_DB: {
                    entity.setCreatedDate(null);
                    processRepository.saveProcessInstance(entity);
                    break;
                }
                default: {
                    throw new IllegalArgumentException("Illegal policy:" + createdDatePolicy);
                }
                }
                addMessageOfInstanceCreate(entity, process, actionList);
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("Update " + entity.getInstanceNo());
                }
                processRepository.updateProcessInstanceByNo(entity);
                addMessageOfInstanceUpdate(entity, process, actionList);
            }
        } else if (o instanceof ProcessNodeInstanceDTO) {
            ProcessNodeInstanceEntity entity = ProcessInstanceDomainConverter.INSTANCE
                    .convert((ProcessNodeInstanceDTO) o);
            if (persistOp == null) {
                ProcessNodeInstanceEntity currentEntity = processRepository
                        .getByNodeInstanceNo(entity.getNodeInstanceNo());
                persistOp = currentEntity == null ? ProcessTransactionConstants.PERSIST_OP_ADD
                        : ProcessTransactionConstants.PERSIST_OP_UPDATE;
            }
            if (persistOp == ProcessTransactionConstants.PERSIST_OP_ADD) {
                if (log.isDebugEnabled()) {
                    log.debug("Add " + entity.getNodeInstanceNo());
                }
                switch (createdDatePolicy) {
                case ProcessConstants.CREATED_DATE_POLICY_CLIENT: {
                    processRepository.saveProcessNodeInstanceWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_SERVER: {
                    entity.setCreatedDate(new Date());
                    processRepository.saveProcessNodeInstanceWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_DB: {
                    entity.setCreatedDate(null);
                    processRepository.saveProcessNodeInstance(entity);
                    break;
                }
                default: {
                    throw new IllegalArgumentException("Illegal policy:" + createdDatePolicy);
                }
                }
                addMessageOfNodeInstanceCreate(entity, process, actionList, sendNodeInstanceStatusMessage, nodeInstanceStatusTopic, engine);
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("Update " + entity.getNodeInstanceNo());
                }
                addMessageOfNodeInstanceUpdate(entity, process, actionList, sendNodeInstanceStatusMessage, nodeInstanceStatusTopic, engine);
                processRepository.updateProcessNodeInstanceByNo(entity);
            }
        } else if (o instanceof ProcessNodeExecutionDTO) {
            if (EXECUTION_PERSIST_TYPE_NONE.equals(executionPeristType)) {
                if (log.isDebugEnabled()) {
                    log.debug("Execution persist type is none, no persist");
                }
                return;
            }
            if (EXECUTION_PERSIST_TYPE_NEXT.equals(executionPeristType)) {
                ProcessNodeExecutionDTO dto = (ProcessNodeExecutionDTO) o;
                if (dto.getNextNodeInstances() == null || dto.getNextNodeInstances().length() == 0) {
                    if (log.isDebugEnabled()) {
                        log.debug("Execution persist type is next, no persist");
                    }
                    return;
                }
            } else if (EXECUTION_PERSIST_TYPE_EVENT_AND_NEXT.equals(executionPeristType)) {
                ProcessNodeExecutionDTO dto = (ProcessNodeExecutionDTO) o;
                if ((dto.getNextNodeInstances() == null || dto.getNextNodeInstances().length() == 0) && dto.getEventId() == null) {
                    if (log.isDebugEnabled()) {
                        log.debug("Execution persist type is event and next, no persist");
                    }
                    return;
                }
            }
            
            Integer finalPersistOp = persistOp;
            Runnable runnable = () -> {
                Integer finalPersistOp2 = finalPersistOp;
                ProcessNodeExecutionEntity entity = ProcessInstanceDomainConverter.INSTANCE
                        .convert((ProcessNodeExecutionDTO) o);
                if (finalPersistOp2 == null) {
                    ProcessNodeExecutionEntity currentEntity = processRepository
                            .getByNodeExecutionNo(entity.getNodeExecutionNo());
                    finalPersistOp2 = currentEntity == null ? ProcessTransactionConstants.PERSIST_OP_ADD
                            : ProcessTransactionConstants.PERSIST_OP_UPDATE;
                }
                if (finalPersistOp2 == ProcessTransactionConstants.PERSIST_OP_ADD) {
                    if (log.isDebugEnabled()) {
                        log.debug("Add " + entity.getNodeExecutionNo());
                    }
                    switch (createdDatePolicy) {
                    case ProcessConstants.CREATED_DATE_POLICY_CLIENT: {
                        processRepository.saveProcessNodeExecutionWithCreatedDate(entity);
                        break;
                    }
                    case ProcessConstants.CREATED_DATE_POLICY_SERVER: {
                        entity.setCreatedDate(new Date());
                        processRepository.saveProcessNodeExecutionWithCreatedDate(entity);
                        break;
                    }
                    case ProcessConstants.CREATED_DATE_POLICY_DB: {
                        entity.setCreatedDate(null);
                        processRepository.saveProcessNodeExecution(entity);
                        break;
                    }
                    default: {
                        throw new IllegalArgumentException("Illegal policy:" + createdDatePolicy);
                    }
                    }
                } else {
                    if (log.isDebugEnabled()) {
                        log.info("Update " + entity.getNodeExecutionNo());
                    }
                    processRepository.updateProcessNodeExecutionByNo(entity);
                }
            };
            if (EXECUTION_PERSIST_POLICY_SYNC.equals(executionPersistPolicy)) {
                runnable.run();
            } else {
                boolean contains = false;
                for (Map<String, Object> action : actionList) {
                    if (ProcessConstants.TXN_ACTION_MERGE_ASYNC.equals(action.get("type"))) {
                        List<Runnable> runnableList = (List<Runnable>) action.get("tasks");
                        if (runnableList != null) {
                            runnableList.add(runnable);
                            contains = true;
                            break;
                        }
                    }
                }
                if (!contains) {
                    Map<String, Object> action = new HashMap<>();
                    action.put("type", ProcessConstants.TXN_ACTION_MERGE_ASYNC);
                    List<Runnable> runnableList = new ArrayList<>();
                    runnableList.add(runnable);
                    action.put("tasks", runnableList);
                    actionList.add(action);
                }
            }
        } else {
            throw new IllegalArgumentException("Illegal object class:" + o.getClass());
        }
    }

    private void addMessageOfInstanceCreate(ProcessInstanceEntity entity, StdProcessContextDTO processContext,
            List<Map<String, Object>> actionList) {
        if (processContext.getProcessProperties() != null
                && processContext.getProcessProperties().get("processInstanceStatusMessage") != null) {
            Map<String, Object> messageConfig = (Map<String, Object>) processContext.getProcessProperties()
                    .get("processInstanceStatusMessage");
            if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                Map<String, Object> createProcessInstanceAction = new HashMap<>();
                createProcessInstanceAction.put("topic",
                        messageConfig.getOrDefault("topic", processInstanceStatusTopic));
                ProcessInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(entity);
                message.setStatus("ACTIVE");
                createProcessInstanceAction.put("bizData", JSON.toJSONString(message));
                createProcessInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                actionList.add(createProcessInstanceAction);
                if ("CLOSE".equals(entity.getStatus())) {
                    Map<String, Object> closeProcessInstanceAction = new HashMap<>();
                    closeProcessInstanceAction.put("topic",
                            messageConfig.getOrDefault("topic", processInstanceStatusTopic));
                    message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(entity);
                    closeProcessInstanceAction.put("bizData", JSON.toJSONString(message));
                    closeProcessInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                    actionList.add(closeProcessInstanceAction);
                }
            }
        }
    }

    private void addMessageOfInstanceUpdate(ProcessInstanceEntity newEntity, StdProcessContextDTO processContext,
            List<Map<String, Object>> actionList) {
        if (processContext.getProcessProperties() != null
                && processContext.getProcessProperties().get("processInstanceStatusMessage") != null) {
            Map<String, Object> messageSendConfig = (Map<String, Object>) processContext.getProcessProperties()
                    .get("processInstanceStatusMessage");
            if (Boolean.TRUE.equals(messageSendConfig.get("enable")) && "CLOSE".equals(newEntity.getStatus())) {
                Map<String, Object> closeProcessInstanceAction = new HashMap<>();
                closeProcessInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                closeProcessInstanceAction.put("topic",
                        messageSendConfig.getOrDefault("topic", processInstanceStatusTopic));
                ProcessInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(newEntity);
                closeProcessInstanceAction.put("bizData", JSON.toJSONString(message));
                actionList.add(closeProcessInstanceAction);
            }
        }
    }
    
  
    private void addMessageOfNodeInstanceCreate(ProcessNodeInstanceEntity entity, StdProcessContextDTO processContext,
            List<Map<String, Object>> actionList, boolean sendNodeInstanceStatusMessage,
            String nodeInstanceStatusTopic, String engine) {
        if (sendNodeInstanceStatusMessage) {
            if ("CLOSE".equals(entity.getStatus())) {
                Map<String, Object> createNodeInstanceAction = new HashMap<>();
                createNodeInstanceAction.put("topic", nodeInstanceStatusTopic);
                ProcessNodeInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(entity);
                String status = "fsm".equals(engine) ? "INACTIVE" : "ACTIVE";
                message.setStatus(status);
                createNodeInstanceAction.put("bizData", JSON.toJSONString(message));
                createNodeInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                actionList.add(createNodeInstanceAction);

                Map<String, Object> closeProcessInstanceAction = new HashMap<>();
                closeProcessInstanceAction.put("topic", nodeInstanceStatusTopic);
                message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(entity);
                closeProcessInstanceAction.put("bizData", JSON.toJSONString(message));
                closeProcessInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                actionList.add(closeProcessInstanceAction);
            } else if ("ACTIVE".equals(entity.getStatus()) && "fsm".equals(engine)) {
                Map<String, Object> inactiveNodeInstanceAction = new HashMap<>();
                inactiveNodeInstanceAction.put("topic", nodeInstanceStatusTopic);
                ProcessNodeInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(entity);
                message.setStatus("INACTIVE");
                inactiveNodeInstanceAction.put("bizData", JSON.toJSONString(message));
                inactiveNodeInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                actionList.add(inactiveNodeInstanceAction);

                Map<String, Object> activeProcessInstanceAction = new HashMap<>();
                activeProcessInstanceAction.put("topic", nodeInstanceStatusTopic);
                message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(entity);
                activeProcessInstanceAction.put("bizData", JSON.toJSONString(message));
                activeProcessInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                actionList.add(activeProcessInstanceAction);                
            }  else {
                Map<String, Object> createNodeInstanceAction = new HashMap<>();
                createNodeInstanceAction.put("topic", nodeInstanceStatusTopic);
                ProcessNodeInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(entity);
                message.setStatus(entity.getStatus());
                createNodeInstanceAction.put("bizData", JSON.toJSONString(message));
                createNodeInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                actionList.add(createNodeInstanceAction);
            }
        }
    }

    private void addMessageOfNodeInstanceUpdate(ProcessNodeInstanceEntity newEntity,
            StdProcessContextDTO processContext, List<Map<String, Object>> actionList,
            boolean sendNodeInstanceStatusMessage, String nodeInstanceStatusTopic, String engine) {
        if (sendNodeInstanceStatusMessage) {
            ProcessNodeInstanceEntity oldEntity = processRepository.getByNodeInstanceNo(newEntity.getNodeInstanceNo());
            if (Objects.equals(oldEntity.getStatus(), newEntity.getStatus())) {
                return;
            }
            if ("CLOSE".equals(newEntity.getStatus()) && "INACTIVE".equals(oldEntity.getStatus()) && "flow".equals(engine)) {
                Map<String, Object> activeNodeInstanceAction = new HashMap<>();
                activeNodeInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                activeNodeInstanceAction.put("topic", nodeInstanceStatusTopic);
                ProcessNodeInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE
                        .convert2Msg(newEntity);
                message.setStatus("ACTIVE");
                activeNodeInstanceAction.put("bizData", JSON.toJSONString(message));
                actionList.add(activeNodeInstanceAction);
                
                Map<String, Object> closeNodeInstanceAction = new HashMap<>();
                closeNodeInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                closeNodeInstanceAction.put("topic", nodeInstanceStatusTopic);
                message = ProcessInstanceDomainConverter.INSTANCE
                        .convert2Msg(newEntity);
                closeNodeInstanceAction.put("bizData", JSON.toJSONString(message));
                actionList.add(closeNodeInstanceAction);
                
            } else {
                Map<String, Object> processNodeInstanceAction = new HashMap<>();
                processNodeInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                processNodeInstanceAction.put("topic", nodeInstanceStatusTopic);
                ProcessNodeInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE
                        .convert2Msg(newEntity);
                processNodeInstanceAction.put("bizData", JSON.toJSONString(message));
                actionList.add(processNodeInstanceAction);
            }
        }
    }

    public void updateProcessInstanceExtData(ProcessInstanceEntity processInstance) {
        ProcessInstanceEntity currentEntity = processRepository.getByProcessInstanceNo(processInstance.getInstanceNo());
        currentEntity.setExtData(processInstance.getExtData());
        processRepository.updateProcessInstanceById(currentEntity);
    }

    public void cancel(CancelProcessInstanceReq cancelProcessInstanceReq) {
        String instanceNo = cancelProcessInstanceReq.getInstanceNo();
        ProcessInstanceEntity processInstance = processRepository.getByProcessInstanceNo(instanceNo);
        String processType = processInstance.getProcessType();
        String bizNo = processInstance.getBizNo();
        String lockProcessId = lockProcessInstance(processType, bizNo);
        try {
            List<Map<String, Object>> postActionList = new ArrayList<>();
            transactionTemplate.executeWithoutResult((status) -> {
                ProcessInstanceEntity processInstanceEntity = processRepository.getByProcessInstanceNo(instanceNo);
                CanCancelProcessInstanceRes canRes = canCancel(
                        ProcessInstanceDomainConverter.INSTANCE.convert(cancelProcessInstanceReq));
                if (!canRes.isCanCancel()) {
                    throw new UserException(MessageUtil.getMessage("easyflow.process.server.tip.cannotCancelForReason", new Object[] {instanceNo, canRes.getReason()}));
                }
                doCancelInstanceTask(processInstanceEntity, cancelProcessInstanceReq, postActionList);
            });
            for (Map<String, Object> action : postActionList) {
                String topic = (String) action.get("topic");
                String bizData = (String) action.get("bizData");
                messageSendService.sendMessage(UUID.randomUUID().toString(), topic, bizData);
            }
        } finally {
            unLockProcessInstance(processType, bizNo, lockProcessId);
        }

    }

    public void interruptOnRuntime(String instanceNo, String interruptUser, Date interruptTime,
            StdProcessContextDTO context, List<Map<String, Object>> postActionList) {
        ProcessInstanceEntity processInstanceEntity = processRepository.getByProcessInstanceNo(instanceNo);
        if (ProcessInstanceConstants.STATUS_CLOSE.equals(processInstanceEntity.getStatus())
                || ProcessInstanceConstants.STATUS_CANCELED.equals(processInstanceEntity.getStatus())) {
            log.info("Process instance end, can not execute interrupt");
            return;
        }
            String extData = appendInterruptInfo2ExtData(processInstanceEntity.getExtData());
            processInstanceEntity.setExtData(extData);
            processInstanceEntity.setStatus(STATUS_CLOSE);
            processRepository.updateProcessInstanceById(processInstanceEntity);
            
            ProcessInstanceStatusMessage processInstanceStatusMessage = ProcessInstanceDomainConverter.INSTANCE
                    .convert2Msg(processInstanceEntity);
            Map<String, Object> createInstanceAction = new HashMap<>(3);
            Map<String, Object> properties = context.getProcessProperties();
            if (properties != null && properties.get("processInstanceStatusMessage") != null) {
                Map<String, Object> processInstanceStatusMessageConf = (Map<String, Object>) properties.get("processInstanceStatusMessage");
                Boolean enabled = (Boolean) processInstanceStatusMessageConf.getOrDefault("enable", false);
                if (Boolean.TRUE.equals(enabled)) {
                    String processInstanceMessageTopic = (String) processInstanceStatusMessageConf.getOrDefault("topic", processInstanceStatusTopic);
                    createInstanceAction.put("topic", processInstanceMessageTopic);
                    createInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                    createInstanceAction.put("bizData", JSON.toJSONString(processInstanceStatusMessage));
                    postActionList.add(createInstanceAction);
                }
            }

            QueryTaskReqVO queryTaskReqVO = new QueryTaskReqVO();
            queryTaskReqVO.setProcessInstanceNo(instanceNo);
            queryTaskReqVO.setStatus(TASK_STATUS_PENDING);
            List<ProcessTaskEntity> processTaskEntities = processTaskRepository.queryTask(queryTaskReqVO);
            if (processTaskEntities != null) {
                log.info("Executing task num:{}", processTaskEntities.size());
                for (ProcessTaskEntity processTaskEntity : processTaskEntities) {
                    processTaskEntity.setStatus(TASK_STATUS_CANCELED);
                    processTaskEntity.setExecuteBizResult(TASK_STATUS_CANCELED);
                    processTaskEntity.setExecutor(interruptUser);
                    processTaskRepository.updateById(processTaskEntity);
                    ProcessTaskEventEntity event = new ProcessTaskEventEntity();
                    event.setTaskNo(processTaskEntity.getTaskNo());
                    event.setEventNo(CodeGenerateHelper.generateCode("PROCESS_TASK_EVENT", "PTE"));
                    event.setEventTime(interruptTime);
                    event.setEventUser(interruptUser);
                    event.setEventTime(new Date());
                    event.setEventType(ProcessTaskConstants.TASK_EVENT_CANCELED);
                    processTaskRepository.save(event);
                    ProcessTaskStatusMessage processTaskStatusMessage = ProcessTaskDomainConverter.INSTANCE
                            .convert2Msg(processTaskEntity);
                    Map<String, Object> createTaskAction = new HashMap<>(3);
                    createTaskAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                    createTaskAction.put("topic", taskStatusTopic);
                    createTaskAction.put("bizData", JSON.toJSONString(processTaskStatusMessage));
                    postActionList.add(createTaskAction);
                }
            } else {
                log.error("Process instance {} has no executing task", instanceNo);
            }
    }

    private void doCancelInstanceTask(ProcessInstanceEntity processInstanceEntity,
            CancelProcessInstanceReq cancelProcessInstanceReq, List<Map<String, Object>> postActionList) {
        String instanceNo = processInstanceEntity.getInstanceNo();
        String extData = appendCancelInfo2ExtData(processInstanceEntity.getExtData(), cancelProcessInstanceReq);
        processInstanceEntity.setExtData(extData);
        processInstanceEntity.setStatus(STATUS_CANCELED);
        processRepository.updateProcessInstanceById(processInstanceEntity);
        ProcessInstanceStatusMessage processInstanceStatusMessage = ProcessInstanceDomainConverter.INSTANCE
                .convert2Msg(processInstanceEntity);
        Map<String, Object> createInstanceAction = new HashMap<>(3);
        ProcessDTO process = processDefinitionDomainService
                .getProcessProperties(processInstanceEntity.getProcessDefId());
        Map<String, Object> properties = process.getProperties();
        Map<String, Object> taskProperties = properties == null ? null : (Map<String, Object>) properties.get("task");
        Map<String, Object> processProperties = properties == null ? null : (Map<String, Object>) properties.get("process");        
        
        if (processProperties != null && processProperties.get("processInstanceStatusMessage") != null) {
            Map<String, Object> processInstanceStatusMessageConf = (Map<String, Object>) processProperties
                    .get("processInstanceStatusMessage");
            Boolean enabled = (Boolean) processInstanceStatusMessageConf.getOrDefault("enable", false);
            if (Boolean.TRUE.equals(enabled)) {
                String processInstanceMessageTopic = (String) processInstanceStatusMessageConf.getOrDefault("topic",
                        processInstanceStatusTopic);
                createInstanceAction.put("topic", processInstanceMessageTopic);
                createInstanceAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                createInstanceAction.put("bizData", JSON.toJSONString(processInstanceStatusMessage));
                postActionList.add(createInstanceAction);
            }
        }

        QueryTaskReqVO queryTaskReqVO = new QueryTaskReqVO();
        queryTaskReqVO.setProcessInstanceNo(instanceNo);
        queryTaskReqVO.setStatus(TASK_STATUS_PENDING);
        List<ProcessTaskEntity> processTaskEntities = processTaskRepository.queryTask(queryTaskReqVO);
        if (processTaskEntities != null) {
            log.info("Executing task num:{}", processTaskEntities.size());
            for (ProcessTaskEntity processTaskEntity : processTaskEntities) {
                processTaskEntity.setStatus(TASK_STATUS_CANCELED);
                processTaskEntity.setExecuteBizResult(TASK_STATUS_CANCELED);
                processTaskEntity.setExecutor(cancelProcessInstanceReq.getCancelUser());
                processTaskRepository.updateById(processTaskEntity);
                ProcessTaskEventEntity event = new ProcessTaskEventEntity();
                event.setTaskNo(processTaskEntity.getTaskNo());
                event.setEventNo(CodeGenerateHelper.generateCode("PROCESS_TASK_EVENT", "PTE"));
                event.setEventTime(cancelProcessInstanceReq.getCancelTime());
                event.setEventUser(cancelProcessInstanceReq.getCancelUser());
                event.setEventTime(new Date());
                event.setEventType(ProcessTaskConstants.TASK_EVENT_CANCELED);
                processTaskRepository.save(event);
                if (taskProperties != null && taskProperties.get("taskStatusMessage") != null) {
                    Map<String, Object> messageConfig = (Map<String, Object>) taskProperties.get("taskStatusMessage");
                    if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                        ProcessTaskStatusMessage processTaskStatusMessage = ProcessTaskDomainConverter.INSTANCE
                                .convert2Msg(processTaskEntity);
                        Map<String, Object> createTaskAction = new HashMap<>(3);
                        createTaskAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                        createTaskAction.put("topic", taskStatusTopic);
                        createTaskAction.put("bizData", JSON.toJSONString(processTaskStatusMessage));
                        postActionList.add(createTaskAction);
                    }
                }
            }
        } else {
            log.info("Process instance {} has no executing task", instanceNo);
        }
    }

    private String appendCancelInfo2ExtData(String extData, CancelProcessInstanceReq cancelProcessInstanceReq) {
        Map<String, Object> extDataMap = null;
        try {
            extDataMap = JSON.parseObject(extData, Map.class);
        } catch (Exception e) {
            log.error("JSON.parseObject parse extData:{} exception", extData);
        }
        if (null == extDataMap) {
            extDataMap = new HashMap<>(1);
        }
        Map<String, Object> cancelInfoMap = new HashMap(4);
        cancelInfoMap.put("cancelUser", cancelProcessInstanceReq.getCancelUser());
        cancelInfoMap.put("cancelTime", cancelProcessInstanceReq.getCancelTime());
        cancelInfoMap.put("reqCancelTime", new Date());
        cancelInfoMap.put("cancelReason", cancelProcessInstanceReq.getCancelReason());
        extDataMap.put("cancelInfo", cancelInfoMap);
        return JSON.toJSONString(extDataMap);
    }
    
    private String appendInterruptInfo2ExtData(String extData) {
        Map<String, Object> extDataMap = null;
        try {
            extDataMap = JSON.parseObject(extData, Map.class);
        } catch (Exception e) {
            log.error("JSON.parseObject parse extData:{} exception", extData);
        }
        if (null == extDataMap) {
            extDataMap = new HashMap<>(1);
        }
        Map<String, Object> interruptInfoMap = new HashMap(1);
        interruptInfoMap.put("interruptOnRuntime", true);
        extDataMap.put("interruptInfo", interruptInfoMap);
        return JSON.toJSONString(extDataMap);
    }
    
    public CanCancelProcessInstanceRes canCancel(CanCancelProcessInstanceReq canCancelProcessInstanceReq) {
        CanCancelProcessInstanceRes res = new CanCancelProcessInstanceRes();
        res.setCanCancel(false);
        String instanceNo = canCancelProcessInstanceReq.getInstanceNo();
        ProcessInstanceEntity currentEntity = queryProcessInstance(instanceNo);
        if (ObjectUtils.isEmpty(currentEntity)) {
            log.error("Process instance not found:{}", instanceNo);
            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotCancelForInstanceNotExists"));
            return res;
        }
        String status = currentEntity.getStatus();
        if (STATUS_CANCELED.equals(status)) {
            log.error("Process instance is cannceled status:{}", instanceNo);
            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotCancelForCanncelStatus"));
            return res;
        }
        if (!STATUS_CLOSE.equals(status)) {
            ProcessDTO process = processDefinitionDomainService.getProcessProperties(currentEntity.getProcessDefId());
            Map<String, Object> properties = process.getProperties();
            Map<String, Object> processProperties = properties == null ? null
                    : (Map<String, Object>) properties.get("process");
            Map<String, Object> instanceCancelProperties = processProperties == null ? null
                    : (Map<String, Object>) processProperties.get("instanceCancel");
            boolean instanceCancelEnable = instanceCancelProperties == null ? false
                    : Boolean.TRUE.equals(instanceCancelProperties.get("enable"));
            if (!instanceCancelEnable) {
                if (log.isDebugEnabled()) {
                    log.debug("Process definition {} not config cancellable", currentEntity.getProcessDefId());
                }
                res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotCancelForNotConfigCancel"));
                return res;
            }
            String cancelUser = canCancelProcessInstanceReq.getCancelUser();
            if (!cancelUser.equals(currentEntity.getCreator())) {
                log.error("Creator {} and cancle user {} inconsistent", currentEntity.getCreator(), cancelUser);
                res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotCancelForUserInconsistent"));
                return res;
            }
        } else {
            log.error("Process instance:{} end, can not cancel", instanceNo);
            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotCancelForInstanceFinish"));
            return res;
        }
        res.setCanCancel(true);
        return res;
    }
    
    public PagerResult<ProcessNodeExecutionEntity> pagerQueryNodeExecution(PagerCondition pagerQueryReq) {
        if (pagerQueryReq.getPageSize() > MAX_PAGE_SIZE) {
            throw new UserException("The max page size is " + MAX_PAGE_SIZE);
        }
        
         return processRepository.pagerQueryNodeExecution(pagerQueryReq);
    }
    
    
    public ProcessRepository getProcessRepository() {
        return processRepository;
    }

    public void setProcessRepository(ProcessRepository processRepository) {
        this.processRepository = processRepository;
    }

    public Locker getLocker() {
        return locker;
    }

    public void setLocker(Locker locker) {
        this.locker = locker;
    }


    public TransactionTemplate getTransactionTemplate() {
        return transactionTemplate;
    }

    public void setTransactionTemplate(TransactionTemplate transactionTemplate) {
        this.transactionTemplate = transactionTemplate;
    }

    public String lockProcessInstance(String processType, String bizNo) {
        return locker.lock("_PROCESS_INSTANCE", getLockKey(processType, bizNo));
    }

    public Boolean unLockProcessInstance(String processType, String bizNo, String lockRequestId) {
        return locker.unlock("_PROCESS_INSTANCE", getLockKey(processType, bizNo), lockRequestId);
    }

    private String getLockKey(String processType, String bizNo) {
        return processType + "__" + bizNo;
    }

    public String getExecutionPersistPolicy() {
        return executionPersistPolicy;
    }

    public void setExecutionPersistPolicy(String executionPersistPolicy) {
        this.executionPersistPolicy = executionPersistPolicy;
    }

    public String getExecutionPeristType() {
        return executionPeristType;
    }

    public void setExecutionPeristType(String executionPeristType) {
        this.executionPeristType = executionPeristType;
    }

    public String getProcessInstanceStatusTopic() {
        return processInstanceStatusTopic;
    }

    public void setProcessInstanceStatusTopic(String processInstanceStatusTopic) {
        this.processInstanceStatusTopic = processInstanceStatusTopic;
    }

    public String getNodeInstanceStatusTopic() {
        return nodeInstanceStatusTopic;
    }

    public void setNodeInstanceStatusTopic(String nodeInstanceStatusTopic) {
        this.nodeInstanceStatusTopic = nodeInstanceStatusTopic;
    }

    public String getTaskStatusTopic() {
        return taskStatusTopic;
    }

    public void setTaskStatusTopic(String taskStatusTopic) {
        this.taskStatusTopic = taskStatusTopic;
    }

    public String getCreatedDatePolicy() {
        return createdDatePolicy;
    }

    public void setCreatedDatePolicy(String createdDatePolicy) {
        this.createdDatePolicy = createdDatePolicy;
    }

    public MessageSendService getMessageSendService() {
        return messageSendService;
    }

    public void setMessageSendService(MessageSendService messageSendService) {
        this.messageSendService = messageSendService;
    }

    public ProcessScheduleDomainService getProcessScheduleDomainService() {
        return processScheduleDomainService;
    }

    public void setProcessScheduleDomainService(ProcessScheduleDomainService processScheduleDomainService) {
        this.processScheduleDomainService = processScheduleDomainService;
    }

    public ProcessDefinitionDomainService getProcessDefinitionDomainService() {
        return processDefinitionDomainService;
    }

    public void setProcessDefinitionDomainService(ProcessDefinitionDomainService processDefinitionDomainService) {
        this.processDefinitionDomainService = processDefinitionDomainService;
    }

    public ProcessTaskRepository getProcessTaskRepository() {
        return processTaskRepository;
    }

    public void setProcessTaskRepository(ProcessTaskRepository processTaskRepository) {
        this.processTaskRepository = processTaskRepository;
    }
    
    
    
}
