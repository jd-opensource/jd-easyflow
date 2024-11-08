package com.jd.easyflow.process.client.task.service;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

import javax.annotation.PostConstruct;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessTaskExport;
import com.jd.easyflow.process.adapter.export.ProcessTransactionExport;
import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskEventDTO;
import com.jd.easyflow.process.adapter.export.dto.task.QueryTaskReq;
import com.jd.easyflow.process.client.runtime.ObjectIdManager;
import com.jd.easyflow.process.client.runtime.ProcessCache;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeManager;
import com.jd.easyflow.process.client.runtime.StdProcessContext;
import com.jd.easyflow.process.client.runtime.core.ProcessException;
import com.jd.easyflow.process.client.task.TaskConstants;
import com.jd.easyflow.process.client.task.TaskErrorCode;
import com.jd.easyflow.process.client.task.biz.TaskBizService;
import com.jd.easyflow.process.client.task.biz.dto.TaskBizParam;
import com.jd.easyflow.process.client.task.biz.dto.TaskBizResult;
import com.jd.easyflow.process.client.task.service.dto.TaskCreateParam;
import com.jd.easyflow.process.client.task.service.dto.TaskExecuteParam;
import com.jd.easyflow.process.client.task.service.operation.ExecuteTaskOperation;
import com.jd.easyflow.process.client.task.service.operation.SaveTaskOperation;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public class TaskClientService {
    
    private static final Logger log = LoggerFactory.getLogger(TaskClientService.class);


    public static final String OP_EXECUTE = "EXECUTE";
    public static final String OP_CREATE = "CREATE";
    public static final String OP_SAVE = "SAVE";

    @Deprecated
    private String defaultExecMode = TaskConstants.EXEC_MODE_CLIENT;

    private ProcessTaskExport processTaskExport;

    private ProcessRuntimeManager processRuntimeManager;

    private ProcessTransactionExport processTransactionExport;

    private TaskClientManager taskClientManager;

    private Map<String, TaskClientOperation> clientOperationMap;

    private ObjectIdManager objectIdManager = ObjectIdManager.INSTANCE;

    @PostConstruct
    public void init() {
        clientOperationMap = new ConcurrentHashMap<>();

        ExecuteTaskOperation executeTaskOperation = new ExecuteTaskOperation();
        executeTaskOperation.setTaskClientManager(taskClientManager);
        clientOperationMap.put(OP_EXECUTE, executeTaskOperation);

        SaveTaskOperation saveTaskOperation = new SaveTaskOperation();
        saveTaskOperation.setTaskClientManager(taskClientManager);
        clientOperationMap.put(OP_SAVE, saveTaskOperation);
    }

    public ProcessRuntimeManager getProcessRuntimeManager() {
        return processRuntimeManager;
    }

    public void setProcessRuntimeManager(ProcessRuntimeManager processRuntimeManager) {
        this.processRuntimeManager = processRuntimeManager;
    }

    public String createTask(TaskCreateParam param) {
        QueryTaskReq query = new QueryTaskReq();
        query.setProcessInstanceNo(param.getProcessContext().getInstanceNo());
        query.setTaskBizCode(param.getTaskBizCode());
        query.setStatus(ProcessTaskConstants.TASK_STATUS_PENDING);
        ExportResponse<List<ProcessTaskDTO>> response = getProcessTaskExport().queryTask(new ExportRequest(query));
        List<ProcessTaskDTO> taskList = ExportResponseUtil.unwrap(response);
        if (taskList.size() == 0) {
            log.info("PENDING task not exists, create new task.");
            invokeCreateBizService(param);
            return doCreateTaskInClientMode(param);
        }
        return null;
    }

    /**
     * 
     * @param param
     */
    private void invokeCreateBizService(TaskCreateParam param) {
        Map<String, Object> createProperties = param.getTaskProperties() == null ? null
                : ((Map<String, Object>) param.getTaskProperties().get("create"));
        String bizService = createProperties == null ? null
                : (String) createProperties.get(TaskConstants.TASK_PROP_BIZ_SERVICE);
        if (StringUtils.isNotEmpty(bizService)) {
            String[] bizServiceInfo = bizService.split(":");
            String providerId = bizServiceInfo.length == 1 ? null : bizServiceInfo[0];
            String serviceId = bizServiceInfo.length == 1 ? bizServiceInfo[0] : bizServiceInfo[1];
            TaskBizService taskBizService = ObjectFactorys.getDefault().getObject(TaskBizService.class, providerId,
                    serviceId);
            Map<String, Object> bizParam = (Map<String, Object>) createProperties
                    .get(TaskConstants.TASK_PROP_BIZ_SERVICE_PARAM);

            TaskBizParam taskCreateParam = new TaskBizParam();
            taskCreateParam.setProcessType(param.getProcessType());
            taskCreateParam.setBizNo(param.getBizNo());
            taskCreateParam.setTaskBizCode(param.getTaskBizCode());
            taskCreateParam.setProcessInstanceNo(param.getProcessContext().getInstanceNo());
            taskCreateParam.setInstanceBizData(param.getInstanceBizData());
            taskCreateParam.setInstanceBizStatus(param.getInstanceBizStatus());
            taskCreateParam.setEvent(TaskConstants.TASK_EVENT_CREATE);
            taskCreateParam.setBizServiceParam(bizParam);

            TaskBizResult result = taskBizService.execute(taskCreateParam);
            if (result != null && result.getCode() != null) {
                Map<String, Object> data = new HashMap<>();
                data.put("bizResCode", result.getCode());
                data.put("bizResDesc", result.getMsg());
                ProcessException exception = new ProcessException(TaskErrorCode.PTC_0101.name(),
                        TaskErrorCode.PTC_0101.getDesc());
                exception.setData(data);
                log.error("Task biz process return fail, fail code:" + result.getCode() + "; reason:" + result.getMsg());
                throw exception;
            }
        }
    }


    private String doCreateTaskInClientMode(TaskCreateParam param) {
        Map<String, Object> createInfoConf = (Map<String, Object>) param.getTaskProperties()
                .get(TaskConstants.TASK_PROP_CREATE);
        Boolean multiple = (Boolean) createInfoConf.get(TaskConstants.TASK_PROP_MULTIPLE);
        if (Boolean.TRUE.equals(multiple)) {
            List<Map<String, Object>> assignInfoConfList = (List<Map<String, Object>>) createInfoConf
                    .get(TaskConstants.TASK_PROP_ASIGNEE_LIST);
            for (Map<String, Object> assignInfoConf : assignInfoConfList) {
                createTaskObject(param, assignInfoConf);
            }
            return null;
        } else {
            Map<String, Object> assignInfoConf = (Map<String, Object>) createInfoConf
                    .get(TaskConstants.TASK_PROP_ASIGNEE);
            ProcessTaskDTO processTask = createTaskObject(param, assignInfoConf);
            return processTask.getTaskNo();

        }
    }


    public ProcessTaskDTO createTaskObject(TaskCreateParam param, Map<String, Object> assignInfoConf) {
        Map<String, Object> assignInfo = computeAssignee(assignInfoConf, param.getElFunction());
        return createTaskObjectForAssignInfo(param, assignInfo);
    }

    public ProcessTaskDTO createTaskObjectForAssignInfo(TaskCreateParam param, Map<String, Object> assignInfo) {
        ProcessCache cache = param.getProcessContext().getCache();
        ProcessTaskDTO task = new ProcessTaskDTO();
        String taskNo = objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_TASK);
        task.setTaskNo(taskNo);
        Date assignTime = new Date();
        task.setAssignTime(assignTime);
        task.setBizNo(param.getBizNo());
        task.setNodeInstanceNo(task.getNodeInstanceNo());
        task.setProcessInstanceNo(param.getProcessContext().getInstanceNo());
        task.setNodeInstanceNo(param.getNodeContext().getNodeInstanceNo());
        task.setProcessType(param.getProcessType());
        task.setProductCode(param.getProductCode());
        task.setTaskType(TaskConstants.TASK_TYPE_MANUAL);
        task.setStatus(ProcessTaskConstants.TASK_STATUS_PENDING);
        task.setTaskBizCode(param.getTaskBizCode());
        task.setTaskBizName(param.getTaskBizName());
        task.setCreator(param.getUser());
        task.setAssignInfo(param.getProductCode());
        task.setAssignInfo(JSON.toJSONString(assignInfo));
        task.setCreatedDate(new Date());
        
        ProcessInstanceDTO processInstance = processRuntimeManager.getProcessInstance(param.getProcessContext());
        task.setProcessInstanceKeyField(processInstance.getKeyField());
        task.setProcessInstanceKeyField2(processInstance.getKeyField2());

        processRuntimeManager.op(param.getProcessContext(), () -> {
            cache.put(task.getTaskNo(), task, true);
            return null;
        });

        List<String> userList = assignInfo == null ? null : (List<String>) assignInfo.get("user");
        List<Object> groupList = assignInfo == null ? null : (List<Object>) assignInfo.get("group");
        List<String> excludeUserList = assignInfo == null ? null : (List<String>) assignInfo.get("excludeUser");        
        if (userList != null && ! userList.isEmpty()) {
            for (String assignUser : userList) {
                if (excludeUserList != null && excludeUserList.contains(assignUser)) {
                    log.info(assignUser + " in excludeUserList");
                    continue;
                }
                ProcessTaskAssignDTO assign = new ProcessTaskAssignDTO();
                String assignNo = objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_TASK_ASSIGN);
                assign.setAssignNo(assignNo);
                assign.setAssignTime(assignTime);
                assign.setAssignType(ProcessTaskConstants.ASSIGN_TYPE_USER);
                assign.setAssignUser(assignUser);
                assign.setProductCode(param.getProductCode());
                assign.setStatus(ProcessTaskConstants.TASK_STATUS_PENDING);
                assign.setTaskNo(taskNo);
                assign.setCreatedDate(new Date());
                cache.put(assign.getAssignNo(), assign, true);
            }
        }

        if (groupList != null && ! groupList.isEmpty()) {
            for (Object group : groupList) {
                ProcessTaskAssignDTO assign = new ProcessTaskAssignDTO();
                String assignNo = objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_TASK_ASSIGN);
                assign.setAssignNo(assignNo);
                assign.setAssignTime(assignTime);
                assign.setAssignType(ProcessTaskConstants.ASSIGN_TYPE_GROUP);
                if (group instanceof String) {
                    assign.setAssignGroup((String) group);
                } else {
                    Map<String, String> groupInfo = (Map<String, String>) group;
                    assign.setAssignGroup(groupInfo.get("group"));
                    assign.setAssignGroup2(groupInfo.get("group2"));
                }
                assign.setProductCode(param.getProductCode());
                assign.setStatus(ProcessTaskConstants.TASK_STATUS_PENDING);
                assign.setTaskNo(taskNo);
                assign.setCreatedDate(new Date());
                cache.put(assign.getAssignNo(), assign, true);
            }
        }

        ProcessTaskEventDTO event = new ProcessTaskEventDTO();
        event.setTaskNo(taskNo);
        event.setEventNo(objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_TASK_EVENT));
        event.setEventTime(assignTime);
        event.setProductCode(param.getProductCode());
        event.setEventType(ProcessTaskConstants.TASK_EVENT_CREATE);
        event.setEventBizData(JSON.toJSONString(task));
        event.setEventUser(param.getUser());
        event.setCreatedDate(new Date());
        cache.put(event.getEventNo(), event, true);
        return task;
    }

    protected Map<String, Object> computeAssignee(Map<String, Object> assignInfoConf,
            Function<String, Object> elFunction) {
        if (assignInfoConf == null) {
            log.info("assign info conf is null");
            return null;
        }

        Map<String, Object> assignInfo = new HashMap<>();
        String userExp = (String) assignInfoConf.get("user");
        if (userExp != null) {
            Object o = elFunction.apply(userExp);
            List<String> userList = null;
            if (o instanceof String) {
                userList = Arrays.asList((String) o);
            } else {
                userList = (List<String>) o;
            }
            if (userList != null && ! userList.isEmpty()) {
                assignInfo.put("user", userList);
            }
        }

        String groupExp = (String) assignInfoConf.get("group");
        if (groupExp != null) {
            Object o = elFunction.apply(groupExp);
            List<Object> groupList = null;
            if (o instanceof String) {
                groupList = Arrays.asList((String) o);
            } else {
                groupList = (List<Object>) o;
            }
            if (groupList != null && ! groupList.isEmpty()) {
                assignInfo.put("group", groupList);
            }
        }

        String excludeUserExp = (String) assignInfoConf.get("excludeUser");
        if (excludeUserExp != null) {
            Object o = elFunction.apply(excludeUserExp);
            List<String> excludeUserList = null;
            if (o instanceof String) {
                excludeUserList = Arrays.asList((String) o);
            } else {
                excludeUserList = (List<String>) o;
            }
            if (excludeUserList != null && ! excludeUserList.isEmpty()) {
                assignInfo.put("excludeUser", excludeUserList);
            }
        }

        return assignInfo;
    }

    public void executeTask(TaskExecuteParam param) {
        invokeExecuteBizService(param);
        doExecuteTaskInClientMode(param);

    }

    private void invokeExecuteBizService(TaskExecuteParam param) {
        Map<String, Object> executeProperties = param.getTaskProperties() == null ? null
                : ((Map<String, Object>) param.getTaskProperties().get("execute"));
        String bizService = executeProperties == null ? null
                : (String) executeProperties.get(TaskConstants.TASK_PROP_BIZ_SERVICE);
        if (StringUtils.isNotEmpty(bizService)) {
            String[] bizServiceInfo = bizService.split(":");
            String providerId = bizServiceInfo.length == 1 ? null : bizServiceInfo[0];
            String serviceId = bizServiceInfo.length == 1 ? bizServiceInfo[0] : bizServiceInfo[1];
            TaskBizService taskBizService = ObjectFactorys.getDefault().getObject(TaskBizService.class, providerId,
                    serviceId);
            Map<String, Object> bizParam = (Map<String, Object>) executeProperties
                    .get(TaskConstants.TASK_PROP_BIZ_SERVICE_PARAM);
            TaskBizParam taskExecuteParam = new TaskBizParam();
            taskExecuteParam.setProcessType(param.getProcessType());
            taskExecuteParam.setBizNo(param.getBizNo());
            taskExecuteParam.setTaskBizCode(param.getTaskBizCode());
            taskExecuteParam.setProcessInstanceNo(param.getProcessContext().getInstanceNo());
            taskExecuteParam.setInstanceBizData(param.getInstanceBizData());
            taskExecuteParam.setInstanceBizStatus(param.getInstanceBizStatus());
            taskExecuteParam.setEvent(TaskConstants.TASK_EVENT_EXECUTE);
            taskExecuteParam.setBizServiceParam(bizParam);
            taskExecuteParam.setOperation(param.getOperation());
            TaskBizResult result = taskBizService.execute(taskExecuteParam);
            if (result != null && result.getCode() != null) {
                Map<String, Object> data = new HashMap<>();
                data.put("bizResCode", result.getCode());
                data.put("bizResDesc", result.getMsg());
                ProcessException exception = new ProcessException(TaskErrorCode.PTC_0101.name(),
                        TaskErrorCode.PTC_0101.getDesc());
                exception.setData(data);
                log.error("Task biz process return fail, fail code:" + result.getCode() + "; reason:" + result.getMsg());
                throw exception;
            }
        }
    }

    public void doExecuteTaskInClientMode(TaskExecuteParam param) {
        String operationId = param.getOperation() == null ? OP_EXECUTE : param.getOperation();
        TaskClientOperation<TaskExecuteParam, Object> operation = clientOperationMap.get(operationId);
        operation.operate(param);
    }

    public List<ProcessTaskDTO> findTaskByNodeInstanceNo(String nodeInstanceNo, Map<String, Object> flowTaskPropertyes,
            StdProcessContext context) {
        return taskClientManager.findTaskByNodeInstanceNo(nodeInstanceNo, context);
    }

    private ProcessTransactionExport getProcessTransactionExport() {
        if (processTransactionExport != null) {
            return processTransactionExport;
        }
        processTransactionExport = ObjectFactorys.getDefault().getObject(ProcessTransactionExport.class);
        return processTransactionExport;
    }

    private ProcessTaskExport getProcessTaskExport() {
        if (processTaskExport == null) {
            processTaskExport = ObjectFactorys.getDefault().getObject(ProcessTaskExport.class);
        }
        return processTaskExport;
    }

    public TaskClientManager getTaskClientManager() {
        return taskClientManager;
    }

    public void setTaskClientManager(TaskClientManager taskClientManager) {
        this.taskClientManager = taskClientManager;
    }

    public void setProcessTaskExport(ProcessTaskExport processTaskExport) {
        this.processTaskExport = processTaskExport;
    }

    public void setProcessTransactionExport(ProcessTransactionExport processTransactionExport) {
        this.processTransactionExport = processTransactionExport;
    }

    public String getDefaultExecMode() {
        return defaultExecMode;
    }

    public void setDefaultExecMode(String defaultExecMode) {
        this.defaultExecMode = defaultExecMode;
    }

    public ObjectIdManager getObjectIdManager() {
        return objectIdManager;
    }

    public void setObjectIdManager(ObjectIdManager objectIdManager) {
        this.objectIdManager = objectIdManager;
    }

}
