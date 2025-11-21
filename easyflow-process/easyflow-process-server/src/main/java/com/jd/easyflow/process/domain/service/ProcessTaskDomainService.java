package com.jd.easyflow.process.domain.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.parser.FlowParser;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.message.MessageSendService;
import com.jd.easyflow.process.adapter.export.constant.ProcessInstanceConstants;
import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.converter.ProcessInstanceConverter;
import com.jd.easyflow.process.adapter.export.converter.ProcessTaskConverter;
import com.jd.easyflow.process.adapter.export.dto.instance.StdProcessContextDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.StdProcessDTO;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskRes;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskEventDTO;
import com.jd.easyflow.process.adapter.message.ProcessNodeInstanceStatusMessage;
import com.jd.easyflow.process.adapter.message.ProcessTaskStatusMessage;
import com.jd.easyflow.process.domain.constant.ProcessConstants;
import com.jd.easyflow.process.domain.constant.ProcessTaskConstants;
import com.jd.easyflow.process.domain.constant.StdProcessConstants;
import com.jd.easyflow.process.domain.converter.ProcessInstanceDomainConverter;
import com.jd.easyflow.process.domain.converter.ProcessTaskDomainConverter;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskAssignEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEventEntity;
import com.jd.easyflow.process.domain.model.vo.ExecuteProcessTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessReqVO;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.process.domain.repository.ProcessTaskRepository;
import com.jd.easyflow.common.util.MessageUtil;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTaskDomainService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessTaskDomainService.class);


    private static final int MAX_PAGE_SIZE = 10000;

    @Resource(name = "easyflow-process-messageSendService")
    private MessageSendService messageSendService;
    @Autowired
    private ProcessScheduleDomainService processScheduleDomainService;
    @Autowired
    private ProcessTaskRepository processTaskRepository;
    @Autowired
    private ProcessInstanceDomainService processInstanceDomainService;

    @Autowired
    private ProcessRepository processRepository;
    @Autowired
    private ProcessDefinitionDomainService processDefinitionDomainService;
    @Resource(name = ProcessConstants.BEAN_NEW_TX_TEMPLATE)
    private TransactionTemplate transactionTemplate;

    @Value(ProcessConstants.CREATED_DATE_POLICY)
    private String createdDatePolicy;

    @Value(ProcessTaskConstants.TOPIC_TASK_STATUS)
    private String taskStatusTopic;
    
    @Value(ProcessConstants.TOPIC_NODE_INSTANCE_STATUS)
    private String nodeInstanceStatusTopic;

    public static final String EVENT_PERSIST_POLICY_SYNC = "SYNC";
    public static final String EVENT_PERSIST_POLICY_ASYNC = "ASYNC";

    public String eventPersistPolicy = EVENT_PERSIST_POLICY_ASYNC;

    private FlowParser flowParser = new FlowParserImpl();

    public List<ProcessTaskEntity> queryTask(QueryTaskReqVO query) {
            return processTaskRepository.queryTask(query);
    }

    public void executeTask(ExecuteProcessTaskReqVO req) {
        AssertUtils.isNotNull(req.getUser(), "User must not be null");

        ProcessTaskEntity processTask = transactionTemplate.execute(status -> {
            return processTaskRepository.getTask(req.getTaskNo());
        });
        ScheduleProcessReqVO scheduleReq = new ScheduleProcessReqVO();
        scheduleReq.setProcessId(null);

        Map<String, Object> param = new HashMap<>();
        param.put(ProcessTaskConstants.PARAM_TASK_EXECUTE_RESULT, req.getExecuteBizResult());
        param.put(ProcessTaskConstants.PARAM_TASK_EXECUTE_DATA, req.getExecuteBizData());
        param.put(ProcessTaskConstants.PARAM_TASK_TASKNO, req.getTaskNo());
        param.put(ProcessTaskConstants.PARAM_TASK_GROUP_LIST, req.getGroupList());
        param.put(ProcessTaskConstants.PARAM_TASK_GROUP2_LIST, req.getGroup2List());
        param.put(ProcessTaskConstants.PARAM_TASK_EXECUTE_CMDLIST_STR, JSON.toJSONString(req.getCmdList()));
        param.put(ProcessTaskConstants.PARAM_TASK_OPERATION, req.getOperation());
        param.put(ProcessTaskConstants.PARAM_TASK_EXT_DATA, req.getTaskExtData());
        scheduleReq.setParam(param);

        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put(StdProcessConstants.PARAM_INSTANCENO, processTask.getProcessInstanceNo());
        dataMap.put(StdProcessConstants.PARAM_PROCESS_TYPE, processTask.getProcessType());
        dataMap.put(StdProcessConstants.PARAM_BIZNO, processTask.getBizNo());
        dataMap.put(StdProcessConstants.PARAM_USER, req.getUser());
        dataMap.put(StdProcessConstants.PARAM_PRODUCT_CODE, processTask.getProductCode());
        dataMap.put(StdProcessConstants.PARAM_BIZ_DATA, req.getInstanceBizData());
        dataMap.put(StdProcessConstants.PARAM_BIZ_STATUS, req.getInstanceBizStatus());
        dataMap.put(FlowConstants.PARAM_DATA_EVENT, "EXECUTE");
        scheduleReq.setDataMap(dataMap);

        scheduleReq.setNodeIds(new String[] { processTask.getTaskBizCode() });
        scheduleReq.setProcessType(processTask.getProcessType());
        scheduleReq.setBizNo(processTask.getBizNo());
        scheduleReq.setProductCode(processTask.getProductCode());
        scheduleReq.setProcessInstanceNo(processTask.getProcessInstanceNo());
        processScheduleDomainService.schedule(scheduleReq);
    }
    
    public CanWithdrawTaskRes canWithdrawTask(String taskNo, String user) {
        CanWithdrawTaskRes res = new CanWithdrawTaskRes();
        res.setCanWithDraw(false);
        ProcessTaskEntity taskEntity = processTaskRepository.getTask(taskNo);
        if (!Objects.equals(taskEntity.getExecutor(), user)) {
            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.taskExecuteUserAndWithdrawUserInconsistent",  new Object[] {taskEntity.getExecutor(), user}));
        }
        if (!com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.TASK_STATUS_FINISH
                .equals(taskEntity.getStatus())) {
            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotWithdrawForTaskNotEnd"));
            return res;
        }
        ProcessInstanceEntity processInstance = processInstanceDomainService
                .queryProcessInstance(taskEntity.getProcessInstanceNo());
        if (ProcessInstanceConstants.STATUS_CLOSE.equals(processInstance.getStatus())) {
            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotWithdrawForInstanceClose"));
            return res;
        }

        ProcessNodeInstanceEntity nodeInstance = processRepository.getByNodeInstanceNo(taskEntity.getNodeInstanceNo());
        String nodeId = nodeInstance.getNodeId();
        ProcessDefinitionEntity processDefinition = processDefinitionDomainService
                .getProcessDefinition(processInstance.getProcessDefId());
        Flow flow = flowParser.parse(processDefinition.getJsonContent(), false).get(0);
        FlowNode flowNode = flow.getNode(nodeId);
        Map<String, Object> taskConfig = flowNode.getProperty(ProcessTaskConstants.NODE_PROP_TASK_KEY);
        Map<String, Object> withdrawConfig = (Map<String, Object>) taskConfig
                .get(ProcessTaskConstants.TASK_PROP_WITHDRAW);
        if (withdrawConfig == null || Boolean.FALSE.equals(withdrawConfig.get(taskEntity))) {
            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotWithdrawForNoConfig", new Object[] {nodeId}));
            return res;
        }
        if (ProcessInstanceConstants.NODE_STATUS_CLOSE.equals(nodeInstance.getStatus())) {
            String[] nextNodeInstances = nodeInstance.getNextNodeInstances() == null ? null
                    : nodeInstance.getNextNodeInstances().split(",");
            if (nextNodeInstances != null && nextNodeInstances.length > 0) {
                for (String nextNodeInstanceNo : nextNodeInstances) {
                    ProcessNodeInstanceEntity nextNodeInstance = processRepository
                            .getByNodeInstanceNo(nextNodeInstanceNo);
                    if (ProcessInstanceConstants.NODE_STATUS_CLOSE.equals(nextNodeInstance.getStatus())) {
                        res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotWithdrawForPostNodeClose", new Object[] {nextNodeInstance.getNodeInstanceNo()}));
                        return res;
                    }
                    if (nextNodeInstance.getPreviousNodeInstances().contains(",")) {
                        res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotWithdrawForMultiplePreNodes", new Object[] {nextNodeInstance.getNodeInstanceNo()}));
                        return res;
                    }
                    QueryTaskReqVO query = new QueryTaskReqVO();
                    query.setNodeInstanceNo(nextNodeInstanceNo);
                    List<ProcessTaskEntity> nextNodeTaskList = processTaskRepository.queryTask(query);
                    for (ProcessTaskEntity nextNodeTask : nextNodeTaskList) {
                        if (!ProcessTaskConstants.TASK_STATUS_PENDING.equals(nextNodeTask.getStatus())) {
                            res.setReason(MessageUtil.getMessage("easyflow.process.server.tip.cannotWithdrawForTaskProcessed", new Object[] {nextNodeTask.getTaskNo()}));
                            return res;
                        }
                    }
                }
            }
        }
        res.setCanWithDraw(true);
        return res;
    }

    public void withdrawTask(String taskNo, String user, String withdrawInstancePolicy, String instanceBizStatus,
            String instanceBizData) {
        ProcessTaskEntity task = processTaskRepository.getTask(taskNo);
        String requestId = processInstanceDomainService.lockProcessInstance(task.getProcessType(),
                task.getBizNo());
        try {
            List<Map<String, Object>> postActionList = new ArrayList<>();
            transactionTemplate.executeWithoutResult((status) -> {
                ProcessTaskEntity taskEntity = processTaskRepository.getTask(taskNo);
                CanWithdrawTaskRes canRes = canWithdrawTask(taskNo, user);
                if (!canRes.isCanWithDraw()) {
                    throw new UserException(MessageUtil.getMessage("easyflow.process.server.tip.cannotWithdrawForReason", new Object[] {taskNo, canRes.getReason()}));
                }
                doWithDrawTask(taskEntity, user, withdrawInstancePolicy, instanceBizStatus, instanceBizData,
                        postActionList);
            });
            for (Map<String, Object> action : postActionList) {
                String topic = (String) action.get("topic");
                String bizData = (String) action.get("bizData");
                messageSendService.sendMessage(UUID.randomUUID().toString(), topic, bizData);
            }
        } finally {
            processInstanceDomainService.unLockProcessInstance(task.getProcessType(), task.getBizNo(),
                    requestId);
        }

    }

    private void doWithDrawTask(ProcessTaskEntity taskEntity, String user, String withdrawInstancePolicy,
            String instanceBizStatus, String instanceBizData, List<Map<String, Object>> postActionList) {
        Date actionTime = new Date();
        ProcessInstanceEntity processInstance = processRepository
                .getByProcessInstanceNo(taskEntity.getProcessInstanceNo());
        ProcessDefinitionEntity processDefinition = processDefinitionDomainService
                .getProcessDefinition(processInstance.getProcessDefId());
        Flow flow = flowParser.parse(processDefinition.getJsonContent(), false).get(0);
        Map<String, Object> taskProperties = (Map<String, Object>) flow.getProperty("task");
        Map<String, Object> processProperties = (Map<String, Object>) flow.getProperty("process");
        ProcessNodeInstanceEntity nodeInstance = processRepository.getByNodeInstanceNo(taskEntity.getNodeInstanceNo());
        if (ProcessInstanceConstants.NODE_STATUS_CLOSE.equals(nodeInstance.getStatus())) {
            String[] nextNodeInstances = nodeInstance.getNextNodeInstances() == null ? null
                    : nodeInstance.getNextNodeInstances().split(",");
            if (nextNodeInstances != null && nextNodeInstances.length > 0) {
                for (String nextNodeInstanceNo : nextNodeInstances) {
                    ProcessNodeInstanceEntity nextNodeInstance = processRepository
                            .getByNodeInstanceNo(nextNodeInstanceNo);
                    nextNodeInstance.setStatus(ProcessInstanceConstants.NODE_STATUS_INVALID);
                    processRepository.updateProcessNodeInstanceById(nextNodeInstance);
                    if (processProperties != null && processProperties.get("processNodeInstanceStatusMessage") != null) {
                        Map<String, Object> messageConfig = (Map<String, Object>) processProperties.get("processNodeInstanceStatusMessage");
                        if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                            Map<String, Object> invalidNodeAction = new HashMap<>();
                            invalidNodeAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                            invalidNodeAction.put("topic", messageConfig.getOrDefault("topic", nodeInstanceStatusTopic));
                            ProcessNodeInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(nextNodeInstance);
                            invalidNodeAction.put("bizData", JSON.toJSONString(message));
                            postActionList.add(invalidNodeAction);
                        }
                    }
                    QueryTaskReqVO query = new QueryTaskReqVO();
                    query.setNodeInstanceNo(nextNodeInstanceNo);
                    List<ProcessTaskEntity> nextNodeTaskList = processTaskRepository.queryTask(query);
                    for (ProcessTaskEntity nextNodeTask : nextNodeTaskList) {
                        nextNodeTask.setStatus(ProcessTaskConstants.TASK_STATUS_INVALID);
                        processTaskRepository.updateById(nextNodeTask);
                        ProcessTaskEventEntity event = new ProcessTaskEventEntity();
                        event.setTaskNo(nextNodeTask.getTaskNo());
                        event.setEventNo(CodeGenerateHelper.generateCode("PROCESS_TASK_EVENT", "PTE"));
                        event.setEventTime(actionTime);
                        event.setEventUser(user);
                        event.setEventType(ProcessTaskConstants.TASK_EVENT_INVALID_FOR_WITHDRAW);
                        event.setProductCode(taskEntity.getProductCode());
                        processTaskRepository.save(event);

                        if (taskProperties != null && taskProperties.get("taskStatusMessage") != null) {
                            Map<String, Object> messageConfig = (Map<String, Object>) taskProperties
                                    .get("taskStatusMessage");
                            if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                                Map<String, Object> createTaskAction = new HashMap<>();
                                createTaskAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                                createTaskAction.put("topic", messageConfig.getOrDefault("topic", taskStatusTopic));
                                ProcessTaskStatusMessage message = ProcessTaskDomainConverter.INSTANCE
                                        .convert2Msg(nextNodeTask);
                                createTaskAction.put("bizData", JSON.toJSONString(message));
                                postActionList.add(createTaskAction);
                            }
                        }

                    }

                }
            }
            nodeInstance.setStatus(ProcessInstanceConstants.NODE_STATUS_ACTIVE);
            nodeInstance.setEndTime(null);
            nodeInstance.setExecutors(null);
            nodeInstance.setNextNodeInstances(null);
            processRepository.updateProcessNodeInstanceById(nodeInstance);
            if (processProperties != null && processProperties.get("processNodeInstanceStatusMessage") != null) {
                Map<String, Object> messageConfig = (Map<String, Object>) processProperties.get("processNodeInstanceStatusMessage");
                if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                    Map<String, Object> activeNodeAction = new HashMap<>();
                    activeNodeAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                    activeNodeAction.put("topic", messageConfig.getOrDefault("topic", nodeInstanceStatusTopic));
                    ProcessNodeInstanceStatusMessage message = ProcessInstanceDomainConverter.INSTANCE.convert2Msg(nodeInstance);
                    activeNodeAction.put("bizData", JSON.toJSONString(message));
                    postActionList.add(activeNodeAction);
                }
            }
        }
        taskEntity.setStatus(ProcessTaskConstants.TASK_STATUS_PENDING);
        taskEntity.setExecutor(null);
        taskEntity.setExecuteBizResult(null);
        taskEntity.setExecuteBizData(null);
        taskEntity.setExecuteTime(null);
        processTaskRepository.updateById(taskEntity);
        if (taskProperties != null && taskProperties.get("taskStatusMessage") != null) {
            Map<String, Object> messageConfig = (Map<String, Object>) taskProperties.get("taskStatusMessage");
            if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                Map<String, Object> createTaskAction = new HashMap<>();
                createTaskAction.put("topic", messageConfig.getOrDefault("topic", taskStatusTopic));
                createTaskAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                ProcessTaskStatusMessage message = ProcessTaskDomainConverter.INSTANCE.convert2Msg(taskEntity);
                createTaskAction.put("bizData", JSON.toJSONString(message));
                postActionList.add(createTaskAction);
            }
        }

        ProcessTaskEventEntity event = new ProcessTaskEventEntity();
        event.setTaskNo(taskEntity.getTaskNo());
        event.setEventNo(CodeGenerateHelper.generateCode("PROCESS_TASK_EVENT", "PTE"));
        event.setEventTime(actionTime);
        event.setEventUser(user);
        event.setEventType(ProcessTaskConstants.TASK_EVENT_WITHDRAW);
        event.setProductCode(taskEntity.getProductCode());
        event.setInstanceBizStatus(instanceBizStatus);
        event.setInstanceBizData(instanceBizData);
        Map<String, Object> eventBizData = new HashMap<>();
        eventBizData.put("withdrawInstancePolicy", withdrawInstancePolicy);
        event.setEventBizData(JSON.toJSONString(eventBizData));
        processTaskRepository.save(event);
        if (com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.WITHDRAW_INSTANCE_POLICY_CUSTOMIZE
                .equals(withdrawInstancePolicy)) {
            processInstance.setBizStatus(instanceBizStatus);
            processInstance.setBizData(instanceBizData);
        } else if (withdrawInstancePolicy == null
                || com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.WITHDRAW_INSTANCE_POLICY_HISTORY
                        .equals(withdrawInstancePolicy)) {
            List<ProcessTaskEventEntity> eventList = processTaskRepository
                    .findTaskEventListByTaskNo(taskEntity.getTaskNo());
            Collections.sort(eventList, (e1, e2) -> e1.getCreatedDate().compareTo(e2.getCreatedDate()));
            for (int i = 0; i < eventList.size(); i++) {
                ProcessTaskEventEntity eventEntity = eventList.get(eventList.size() - 1 - i);
                if (ProcessTaskConstants.TASK_EVENT_EXECUTE.equals(eventEntity.getEventType())) {
                    String eventInstanceBizStatus = eventEntity.getInstanceBizStatus();
                    String eventInstanceBizData = eventEntity.getInstanceBizData();
                    log.info("With draw process instance data, instanceBizStatus:" + eventInstanceBizStatus + " instanceBizData:"
                            + eventInstanceBizData);
                    if (eventInstanceBizStatus != null && ! eventInstanceBizStatus.isEmpty()) {
                        processInstance.setBizStatus(eventInstanceBizStatus);
                    }
                    if (eventInstanceBizData != null && ! eventInstanceBizData.isEmpty()) {
                        processInstance.setBizData(eventInstanceBizData);
                    }
                }
            }
        } else if (withdrawInstancePolicy.equals(
                com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.WITHDRAW_INSTANCE_POLICY_NONE)) {
            // NOOP
        } else {
            throw new UnsupportedOperationException("Unsupported instance withdraw policy:" + withdrawInstancePolicy);
        }
        List<ProcessNodeInstanceEntity> nodeInstanceList = processRepository
                .findOpenNodeInstances(taskEntity.getProcessInstanceNo());
        Set<String> openNodeIds = new HashSet<String>();
        for (ProcessNodeInstanceEntity node : nodeInstanceList) {
            openNodeIds.add(node.getNodeId());
        }
        processInstance.setCurrentNodeIds(String.join( ",", openNodeIds));
        log.info("Process instance info after withdraw:" + processInstance);
        processRepository.updateProcessInstanceById(processInstance);

    }
    
    public PagerResult<ProcessTaskDTO> pagerQueryTask(PagerCondition condition) {
        if (condition.getPageSize() > MAX_PAGE_SIZE) {
            throw new UserException("The max page size is " + MAX_PAGE_SIZE);
        }
        FieldEntry statusListEntry = condition.getField("statusList");
        FieldEntry statusFieldEntry = condition.getField("status");
        if (statusListEntry == null && statusFieldEntry == null) {
            condition.addField(new FieldEntry("statusList",
                    Arrays.asList(ProcessTaskConstants.TASK_STATUS_FINISH, ProcessTaskConstants.TASK_STATUS_PENDING)));
        }
        PagerResult pagerResult = processTaskRepository.pagerQueryTask(condition);
        List<ProcessTaskDTO> list = ProcessTaskConverter.INSTANCE.convert(pagerResult.getList());
        pagerResult.setList(list);
        if (com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.PAGER_EXT_ASSIGN_PENDING
                .equals(condition.getExtData(
                        com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.PAGER_EXT_KEY_ASSIGN))) {
            List<String> taskNoList = list.stream()
                    .filter(dto -> ProcessTaskConstants.TASK_STATUS_PENDING.equals(dto.getStatus()))
                    .map(dto -> dto.getTaskNo()).collect(Collectors.toList());
            if (taskNoList != null && ! taskNoList.isEmpty()) {
                List<ProcessTaskAssignEntity> assignList = processTaskRepository
                        .findTaskAssignListByTaskNoList(taskNoList);
                Map<String, List<ProcessTaskAssignEntity>> map = assignList.stream()
                        .collect(Collectors.groupingBy(ProcessTaskAssignEntity::getTaskNo));
                list.forEach(task -> task
                        .setAssignList(ProcessTaskConverter.INSTANCE.convertAssignList(map.get(task.getTaskNo()))));
            }
        }
        if (com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.PAGER_EXT_NODE_TASK_CONF_PENDING
                .equals(condition.getExtData(
                        com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.PAGER_EXT_KEY_NODE_TASK_CONF))) {
            for (ProcessTaskDTO task : list) {
                if (ProcessTaskConstants.TASK_STATUS_PENDING.equals(task.getStatus())) {
                    task.setNodeTaskConf(getTaskConf(task));
                }
            }
        }
        if (com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.PAGER_EXT_PROCESS_INSTANCE_ALL
                .equals(condition.getExtData(
                        com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants.PAGER_EXT_KEY_PROCESS_INSTANCE))) {
            for (ProcessTaskDTO task : list) {
                ProcessInstanceEntity processInstance = processInstanceDomainService
                        .queryProcessInstance(task.getProcessInstanceNo());
                task.setProcessInstance(ProcessInstanceConverter.INSTANCE.convert(processInstance));
            }
        }
        return pagerResult;
    }

    private String getTaskConf(ProcessTaskDTO task) {
        String processInstanceNo = task.getProcessInstanceNo();
        ProcessInstanceEntity processInstance = processInstanceDomainService.queryProcessInstance(processInstanceNo);
        ProcessNodeInstanceEntity processNodeInstance = processRepository.getByNodeInstanceNo(task.getNodeInstanceNo());
        ProcessDefinitionEntity processDefinitionEntity = processDefinitionDomainService
                .getProcessDefinition(processInstance.getProcessDefId());
        if (processDefinitionEntity == null) {
            log.warn("process:" + processInstance.getProcessDefId() + " is null");
            return null;
        }
        Flow flow = flowParser.parse(processDefinitionEntity.getJsonContent(), false).get(0);
        FlowNode flowNode = flow.getNode(processNodeInstance.getNodeId());
        Map<String, Object> taskConf = flowNode.getProperty("task");
        return JSON.toJSONString(taskConf);
    }

    public ProcessTaskDTO getTask(String taskNo) {
        AssertUtils.isNotNull(taskNo, "Task no must not be null");
        ProcessTaskEntity taskEntity = processTaskRepository.getTask(taskNo);
        if (taskEntity == null) {
            return null;
        }
        List<ProcessTaskAssignEntity> taskAssignList = processTaskRepository.findTaskAssignListByTaskNo(taskNo);
        List<ProcessTaskAssignDTO> assignDtoList = ProcessTaskConverter.INSTANCE.convertAssignList(taskAssignList);
        ProcessTaskDTO dto = ProcessTaskConverter.INSTANCE.convert(taskEntity);
        dto.setAssignList(assignDtoList);
        ProcessInstanceEntity processInstance = processInstanceDomainService
                .queryProcessInstance(dto.getProcessInstanceNo());
        dto.setProcessInstance(ProcessInstanceConverter.INSTANCE.convert(processInstance));
        return dto;
    }

    public void updateTaskObject(Integer persistOp, Object o, StdProcessContextDTO process,
            List<Map<String, Object>> actionList) {
        if (o instanceof ProcessTaskDTO) {
            ProcessTaskEntity entity = ProcessTaskConverter.INSTANCE.convert((ProcessTaskDTO) o);
            if (persistOp == null) {
                ProcessTaskEntity currentEntity = processTaskRepository.getTask(entity.getTaskNo());
                persistOp = currentEntity == null ? ProcessTransactionConstants.PERSIST_OP_ADD
                        : ProcessTransactionConstants.PERSIST_OP_UPDATE;
            }
            if (persistOp == ProcessTransactionConstants.PERSIST_OP_ADD) {
                if (log.isDebugEnabled()) {
                    log.debug("Add " + entity.getTaskNo());
                }
                switch (createdDatePolicy) {
                case ProcessConstants.CREATED_DATE_POLICY_CLIENT: {
                    processTaskRepository.saveWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_SERVER: {
                    entity.setCreatedDate(new Date());
                    processTaskRepository.saveWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_DB: {
                    entity.setCreatedDate(null);
                    processTaskRepository.save(entity);
                    break;
                }
                default: {
                    throw new IllegalArgumentException("Illegal policy " + createdDatePolicy);
                }
                }
                addMessageOfTaskCreate(entity, process, actionList);
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("Update " + entity.getTaskNo());
                }
                processTaskRepository.updateByTaskNo(entity);
                addMessageOfTaskUpdate(entity, process, actionList);
            }
        } else if (o instanceof ProcessTaskAssignDTO) {
            ProcessTaskAssignEntity entity = ProcessTaskConverter.INSTANCE.convert((ProcessTaskAssignDTO) o);
            if (persistOp == null) {
                ProcessTaskAssignEntity currentEntity = processTaskRepository.getTaskAssign(entity.getAssignNo());
                persistOp = currentEntity == null ? ProcessTransactionConstants.PERSIST_OP_ADD
                        : ProcessTransactionConstants.PERSIST_OP_UPDATE;
            }
            if (persistOp == ProcessTransactionConstants.PERSIST_OP_ADD) {
                if (log.isDebugEnabled()) {
                    log.debug("Add " + entity.getAssignNo());
                }
                switch (createdDatePolicy) {
                case ProcessConstants.CREATED_DATE_POLICY_CLIENT: {
                    processTaskRepository.saveWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_SERVER: {
                    entity.setCreatedDate(new Date());
                    processTaskRepository.saveWithCreatedDate(entity);
                    break;
                }
                case ProcessConstants.CREATED_DATE_POLICY_DB: {
                    entity.setCreatedDate(null);
                    processTaskRepository.save(entity);
                    break;
                }
                default: {
                    throw new IllegalArgumentException("Illegal policy:" + createdDatePolicy);
                }
                }
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("Update" + entity.getAssignNo());
                }
                processTaskRepository.updateByTaskAssignNo(entity);
            }
        } else if (o instanceof ProcessTaskEventDTO) {
            Integer finalPersistOp = persistOp;
            Runnable runnable = () -> {
                Integer finalPersistOp2 = finalPersistOp;
                ProcessTaskEventEntity entity = ProcessTaskConverter.INSTANCE.convert((ProcessTaskEventDTO) o);
                if (finalPersistOp2 == null) {
                    ProcessTaskEventEntity currentEntity = processTaskRepository.getTaskEvent(entity.getEventNo());
                    finalPersistOp2 = currentEntity == null ? ProcessTransactionConstants.PERSIST_OP_ADD
                            : ProcessTransactionConstants.PERSIST_OP_UPDATE;
                }
                if (finalPersistOp2 == ProcessTransactionConstants.PERSIST_OP_ADD) {
                    if (log.isDebugEnabled()) {
                        log.debug("Add " + entity.getEventNo());
                    }
                    switch (createdDatePolicy) {
                    case ProcessConstants.CREATED_DATE_POLICY_CLIENT: {
                        processTaskRepository.saveWithCreatedDate(entity);
                        break;
                    }
                    case ProcessConstants.CREATED_DATE_POLICY_SERVER: {
                        entity.setCreatedDate(new Date());
                        processTaskRepository.saveWithCreatedDate(entity);
                        break;
                    }
                    case ProcessConstants.CREATED_DATE_POLICY_DB: {
                        entity.setCreatedDate(null);
                        processTaskRepository.save(entity);
                        break;
                    }
                    default: {
                        throw new IllegalArgumentException("Illegal policy:" + createdDatePolicy);
                    }
                    }
                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("Update " + entity.getEventNo());
                    }
                    processTaskRepository.updateByTaskEventNo(entity);
                }
            };

            if (EVENT_PERSIST_POLICY_SYNC.equals(eventPersistPolicy)) {
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

    private void addMessageOfTaskCreate(ProcessTaskEntity entity, StdProcessContextDTO processContext,
            List<Map<String, Object>> actionList) {
        Map<String, Object> taskProperties = null;
        StdProcessDTO process = processContext.getProcess();
        if (process != null && process.getExtProperties() != null) {
            taskProperties = (Map<String, Object>) process.getExtProperties().get("task");
        }
        if (taskProperties != null && taskProperties.get("taskStatusMessage") != null) {
            Map<String, Object> messageConfig = (Map<String, Object>) taskProperties.get("taskStatusMessage");
            if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                Map<String, Object> createTaskAction = new HashMap<>();
                createTaskAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                createTaskAction.put("topic", messageConfig.getOrDefault("topic", taskStatusTopic));
                ProcessTaskStatusMessage message = ProcessTaskDomainConverter.INSTANCE.convert2Msg(entity);
                message.setStatus(ProcessTaskConstants.TASK_STATUS_PENDING);
                createTaskAction.put("bizData", JSON.toJSONString(message));
                actionList.add(createTaskAction);
                if (ProcessTaskConstants.TASK_STATUS_FINISH.equals(entity.getStatus())
                        || ProcessTaskConstants.TASK_STATUS_INVALID.equals(entity.getStatus())) {
                    Map<String, Object> endTaskAction = new HashMap<>();
                    endTaskAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                    endTaskAction.put("topic", messageConfig.getOrDefault("topic", taskStatusTopic));
                    message = ProcessTaskDomainConverter.INSTANCE.convert2Msg(entity);
                    endTaskAction.put("bizData", JSON.toJSONString(message));
                    actionList.add(endTaskAction);
                }
            }
        }
    }

    private void addMessageOfTaskUpdate(ProcessTaskEntity entity, StdProcessContextDTO processContext,
            List<Map<String, Object>> actionList) {
        Map<String, Object> taskProperties = null;
        StdProcessDTO process = processContext.getProcess();
        if (process != null && process.getExtProperties() != null) {
            taskProperties = (Map<String, Object>) process.getExtProperties().get("task");
        }
        if (taskProperties != null && taskProperties.get("taskStatusMessage") != null) {
            Map<String, Object> messageConfig = (Map<String, Object>) taskProperties.get("taskStatusMessage");
            if (Boolean.TRUE.equals(messageConfig.get("enable"))) {
                Map<String, Object> createTaskAction = new HashMap<>();
                createTaskAction.put("type", ProcessConstants.TXN_ACTION_MSG);
                createTaskAction.put("topic", messageConfig.getOrDefault("topic", taskStatusTopic));
                ProcessTaskStatusMessage message = ProcessTaskDomainConverter.INSTANCE.convert2Msg(entity);
                createTaskAction.put("bizData", JSON.toJSONString(message));
                actionList.add(createTaskAction);
            }
        }
    }
    
    public TransactionTemplate getTransactionTemplate() {
        return transactionTemplate;
    }

    public void setTransactionTemplate(TransactionTemplate transactionTemplate) {
        this.transactionTemplate = transactionTemplate;
    }

    public MessageSendService getMessageSendService() {
        return messageSendService;
    }

    public void setMessageSendService(MessageSendService messageSendService) {
        this.messageSendService = messageSendService;
    }

    public String getTaskStatusTopic() {
        return taskStatusTopic;
    }

    public void setTaskStatusTopic(String taskStatusTopic) {
        this.taskStatusTopic = taskStatusTopic;
    }

    public String getEventPersistPolicy() {
        return eventPersistPolicy;
    }

    public void setEventPersistPolicy(String eventPersistPolicy) {
        this.eventPersistPolicy = eventPersistPolicy;
    }

    public String getNodeInstanceStatusTopic() {
        return nodeInstanceStatusTopic;
    }

    public void setNodeInstanceStatusTopic(String nodeInstanceStatusTopic) {
        this.nodeInstanceStatusTopic = nodeInstanceStatusTopic;
    }

    public ProcessScheduleDomainService getProcessScheduleDomainService() {
        return processScheduleDomainService;
    }

    public void setProcessScheduleDomainService(ProcessScheduleDomainService processScheduleDomainService) {
        this.processScheduleDomainService = processScheduleDomainService;
    }

    public ProcessTaskRepository getProcessTaskRepository() {
        return processTaskRepository;
    }

    public void setProcessTaskRepository(ProcessTaskRepository processTaskRepository) {
        this.processTaskRepository = processTaskRepository;
    }

    public ProcessInstanceDomainService getProcessInstanceDomainService() {
        return processInstanceDomainService;
    }

    public void setProcessInstanceDomainService(ProcessInstanceDomainService processInstanceDomainService) {
        this.processInstanceDomainService = processInstanceDomainService;
    }

    public ProcessRepository getProcessRepository() {
        return processRepository;
    }

    public void setProcessRepository(ProcessRepository processRepository) {
        this.processRepository = processRepository;
    }

    public ProcessDefinitionDomainService getProcessDefinitionDomainService() {
        return processDefinitionDomainService;
    }

    public void setProcessDefinitionDomainService(ProcessDefinitionDomainService processDefinitionDomainService) {
        this.processDefinitionDomainService = processDefinitionDomainService;
    }

    public String getCreatedDatePolicy() {
        return createdDatePolicy;
    }

    public void setCreatedDatePolicy(String createdDatePolicy) {
        this.createdDatePolicy = createdDatePolicy;
    }

    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }
    
    

}
