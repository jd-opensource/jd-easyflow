package com.jd.easyflow.process.client.task.service.operation;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessTransactionExport;
import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskEventDTO;
import com.jd.easyflow.process.client.runtime.ObjectIdManager;
import com.jd.easyflow.process.client.runtime.ProcessCache;
import com.jd.easyflow.process.client.task.TaskConstants;
import com.jd.easyflow.process.client.task.service.TaskClientManager;
import com.jd.easyflow.process.client.task.service.TaskClientOperation;
import com.jd.easyflow.process.client.task.service.dto.TaskCreateParam;
import com.jd.easyflow.process.client.task.service.dto.TaskExecuteParam;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class BaseTaskOperation<P, R> implements TaskClientOperation<P, R> {
    
    private static final Logger log = LoggerFactory.getLogger(BaseTaskOperation.class);


    protected TaskClientManager taskClientManager;

    private ProcessTransactionExport processTransactionExport;

    private ObjectIdManager objectIdManager = ObjectIdManager.INSTANCE;


    protected ProcessTaskDTO createTaskObjectForAssignInfo(TaskCreateParam param, Map<String, Object> assignInfo) {
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
        taskClientManager.getProcessRuntimeManager().op(param.getProcessContext(), () -> { 
            cache.put(task.getTaskNo(), task, true);
        return null;
        });
        

        List<String> userList = assignInfo == null ? null : (List<String>) assignInfo.get("user");
        List<Object> groupList = assignInfo == null ? null : (List<Object>) assignInfo.get("group");
        List<String> excludeUserList = assignInfo == null ? null : (List<String>) assignInfo.get("excludeUser");
        if (userList != null && ! userList.isEmpty()) {
            for (String assignUser : userList) {
                if (excludeUserList != null && excludeUserList.contains(assignUser)) {
                    log.info(assignUser + " in exclude user list");
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

    protected void validateTaskExecuteAuth(TaskExecuteParam param, ProcessTaskDTO task) {
        String executor = param.getUser();
        List<ProcessTaskAssignDTO> assignList = taskClientManager.findTaskAssignListByTaskNo(param.getTaskNo(),
                param.getProcessContext());
        boolean pass = false;
        for (ProcessTaskAssignDTO assign : assignList) {
            if (ProcessTaskConstants.ASSIGN_TYPE_USER.equals(assign.getAssignType())) {
                if (StringUtils.equals(assign.getAssignUser(), executor)) {
                    pass = true;
                    break;
                }
            } else {
                if ((assign.getAssignGroup() == null
                        || param.getGroupList() != null && param.getGroupList().contains(assign.getAssignGroup()))
                        && ((assign.getAssignGroup2() == null || param.getGroup2List() != null
                                && param.getGroup2List().contains(assign.getAssignGroup2())))) {
                    pass = true;
                    break;
                }
            }
        }
        String assignInfo = task.getAssignInfo();
        Map<String, Object> assignInfoMap = JSON.parseObject(assignInfo, Map.class);
        List<String> excludeUser = (List<String>) assignInfoMap.get("excludeUser");
        if (excludeUser != null && excludeUser.contains(executor)) {
            log.warn("use {} in excludeUser", executor);
            pass = false;
        }

        if (!pass) {
            throw new EasyFlowException("Auth validate fail");
        }

    }

    protected void createTaskExecuteEvent(TaskExecuteParam param, ProcessTaskDTO task) {
        ProcessCache cache = param.getProcessContext().getCache();
        ProcessTaskEventDTO event = new ProcessTaskEventDTO();
        event.setTaskNo(param.getTaskNo());
        event.setEventNo(objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_TASK_EVENT));
        event.setEventTime(new Date());
        event.setEventUser(param.getUser());
        event.setEventType(ProcessTaskConstants.TASK_EVENT_EXECUTE);
        event.setProductCode(task.getProductCode());
        event.setEventBizResult(param.getExecuteBizResult());
        Map<String, Object> bizData = new HashMap<>();
        bizData.put("executor", param.getUser());
        bizData.put("groupList", param.getGroupList());
        bizData.put("group2List", param.getGroup2List());
        bizData.put("executeBizResult", param.getExecuteBizResult());
        bizData.put("executeBizData", param.getExecuteBizData());
        bizData.put("newTaskExtData", task.getExtData());
        bizData.put("taskExtData", param.getTaskExtData());
        bizData.put("instanceBizStatus", param.getInstanceBizStatus());
        bizData.put("instanceBizData", param.getInstanceBizData());
        bizData.put("cmdListStr", param.getCmdListStr());
        bizData.put("operation", param.getOperation());
        event.setEventBizData(JSON.toJSONString(bizData));
        event.setInstanceBizStatus(param.getInstanceBizStatus());
        event.setInstanceBizData(param.getInstanceBizData());
        event.setCreatedDate(new Date());
        taskClientManager.getProcessRuntimeManager().op(param.getProcessContext(), () -> { 
        cache.put(event.getEventNo(), event, true);
        return null;
        });
    }

    protected ProcessTransactionExport getProcessTransactionExport() {
        if (processTransactionExport != null) {
            return processTransactionExport;
        }
        processTransactionExport = ObjectFactorys.getDefault().getObject(ProcessTransactionExport.class);
        return processTransactionExport;
    }

    public TaskClientManager getTaskClientManager() {
        return taskClientManager;
    }

    public void setTaskClientManager(TaskClientManager taskClientManager) {
        this.taskClientManager = taskClientManager;
    }

    public ObjectIdManager getObjectIdManager() {
        return objectIdManager;
    }

    public void setObjectIdManager(ObjectIdManager objectIdManager) {
        this.objectIdManager = objectIdManager;
    }

}
