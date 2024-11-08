package com.jd.easyflow.process.client.task.service.operation;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.cmd.TaskCreateCmd;
import com.jd.easyflow.process.client.runtime.ProcessCache;
import com.jd.easyflow.process.client.task.service.dto.TaskCreateParam;
import com.jd.easyflow.process.client.task.service.dto.TaskExecuteParam;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class ExecuteTaskOperation extends BaseTaskOperation<TaskExecuteParam, Object> {
    
    private static final Logger log = LoggerFactory.getLogger(ExecuteTaskOperation.class);

    
    @Override
    public Object operate(TaskExecuteParam param) {
        ProcessCache cache = param.getProcessContext().getCache();
        Date eventTime = new Date();
        ProcessTaskDTO task = taskClientManager.getTask(param.getTaskNo(), param.getProcessContext());
        if (!ProcessTaskConstants.TASK_STATUS_PENDING.equals(task.getStatus())) {
            throw new EasyFlowException("Task " + param.getTaskNo() + " not PENDING status");
        }
        validateTaskExecuteAuth(param, task);
        String executor = param.getUser();
        task.setExecutor(executor);
        task.setExecuteTime(eventTime);
        task.setExecuteBizResult(param.getExecuteBizResult());
        task.setExecuteBizData(param.getExecuteBizData());
        task.setStatus(ProcessTaskConstants.TASK_STATUS_FINISH);
        if (param.getTaskExtData() != null) {
            String originalTaskExtData = task.getExtData();
            Map<String, Object> currentTaskExtData = JSON.parseObject(originalTaskExtData, Map.class);
            Map<String, Object> taskExtData = JSON.parseObject(param.getTaskExtData(), Map.class);
            if (currentTaskExtData == null) {
                currentTaskExtData = new HashMap<>();
            }
            currentTaskExtData.putAll(taskExtData);
            task.setExtData(JSON.toJSONString(currentTaskExtData));
        }
        taskClientManager.getProcessRuntimeManager().op(param.getProcessContext(), () -> { 
            cache.put(task.getTaskNo(), task, true);
        return null;
        });
        
        createTaskExecuteEvent(param, task);

        List<Map<String, Object>> cmdMapList = JSON.parseObject(param.getCmdListStr(), List.class);

        if (cmdMapList != null) {
            for (Map<String, Object> cmdMap : cmdMapList) {
                String cmdType = (String) cmdMap.get("cmdType");
                if (ProcessTaskConstants.CMD_TYPE_CREATE_TASK.equals(cmdType)) {
                    TaskCreateCmd createCmd = JSON.parseObject(cmdMap, TaskCreateCmd.class);
                    Map<String, Object> createAssignInfo = createCmd.getAssignInfo();
                    TaskCreateParam createParam = new TaskCreateParam();
                    createParam.setBizNo(task.getBizNo());
                    createParam.setProcessContext(param.getProcessContext());
                    createParam.setProcessType(task.getProcessType());
                    createParam.setProductCode(task.getProductCode());
                    createParam.setTaskBizCode(task.getTaskBizCode());
                    createParam.setTaskBizName(task.getTaskBizName());
                    createParam.setUser(param.getUser());
                    createParam.setProcessContext(param.getProcessContext());
                    createParam.setNodeContext(param.getNodeContext());
                    createTaskObjectForAssignInfo(createParam, createAssignInfo);
                } else {
                    throw new UnsupportedOperationException("Unsupported operation type:" + cmdType);
                }
            }
        }
        return null;
    }
}
