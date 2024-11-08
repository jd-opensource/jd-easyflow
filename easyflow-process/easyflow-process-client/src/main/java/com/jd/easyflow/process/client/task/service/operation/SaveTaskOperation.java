package com.jd.easyflow.process.client.task.service.operation;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.client.runtime.ProcessCache;
import com.jd.easyflow.process.client.task.service.dto.TaskExecuteParam;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public class SaveTaskOperation extends BaseTaskOperation<TaskExecuteParam, Object> {

    @Override
    public Object operate(TaskExecuteParam param) {
        ProcessCache cache = param.getProcessContext().getCache();
        ProcessTaskDTO task = taskClientManager.getTask(param.getTaskNo(), param.getProcessContext());

        if (!ProcessTaskConstants.TASK_STATUS_PENDING.equals(task.getStatus())) {
            throw new EasyFlowException("Task " + param.getTaskNo() + " not PENDING status");
        }

        validateTaskExecuteAuth(param, task);

        task.setExecuteBizResult(param.getExecuteBizResult());
        task.setExecuteBizData(param.getExecuteBizData());
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
        return null;
    }

}
