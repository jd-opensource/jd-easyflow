package com.jd.easyflow.process.client.task.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessTaskExport;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskEventDTO;
import com.jd.easyflow.process.adapter.export.dto.task.QueryTaskReq;
import com.jd.easyflow.process.client.runtime.ProcessCache;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeManager;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class TaskClientManager {

    ProcessRuntimeManager processRuntimeManager;

    ProcessTaskExport processTaskExport;

    public ProcessTaskDTO getTask(String taskNo, StdProcessContext context) {
        return processRuntimeManager.op(context, () -> {
            ProcessCache cache = context.getCache();
            ProcessTaskDTO task = cache.get(ProcessTaskDTO.class, taskNo);
            if (task != null) {
                return task;
            }
            ExportResponse<ProcessTaskDTO> response = getProcessTaskExport().getTask(new ExportRequest(taskNo));
            task = ExportResponseUtil.unwrap(response);
            context.getCache().put(taskNo, task, false);
            return task;
        });
    }

    public List<ProcessTaskDTO> findTaskByNodeInstanceNo(String nodeInstanceNo, StdProcessContext context) {
        return processRuntimeManager.op(context, () -> {
            ProcessCache cache = context.getCache();
            QueryTaskReq query = new QueryTaskReq();
            query.setNodeInstanceNo(nodeInstanceNo);
            List<ProcessTaskDTO> taskList = ExportResponseUtil
                    .unwrap(getProcessTaskExport().queryTask(new ExportRequest<QueryTaskReq>(query)));
            for (ProcessTaskDTO task : taskList) {
                if (cache.get(ProcessTaskDTO.class, task.getTaskNo()) == null) {
                    cache.put(task.getTaskNo(), task, false);
                }
            }
            List<ProcessTaskDTO> result = new ArrayList<>();
            for (ProcessTaskDTO task : cache.objects(ProcessTaskDTO.class)) {
                if (Objects.equals(nodeInstanceNo, task.getNodeInstanceNo())) {
                    result.add(task);
                }
            }
            return result;
        });
    }

    public List<ProcessTaskAssignDTO> findTaskAssignListByTaskNo(String taskNo, StdProcessContext context) {
        return processRuntimeManager.op(context, () -> {
            ProcessCache cache = context.getCache();
            ExportResponse<List<ProcessTaskAssignDTO>> response = getProcessTaskExport()
                    .findTaskAssignListByTaskNo(new ExportRequest(taskNo));
            List<ProcessTaskAssignDTO> assignList = ExportResponseUtil.unwrap(response);
            for (ProcessTaskAssignDTO assign : assignList) {
                if (cache.get(ProcessTaskAssignDTO.class, assign.getAssignNo()) == null) {
                    cache.put(assign.getAssignNo(), assign, false);
                }
            }
            List<ProcessTaskAssignDTO> result = new ArrayList<>();
            for (ProcessTaskAssignDTO assign : cache.objects(ProcessTaskAssignDTO.class)) {
                if (assign.getTaskNo().equals(taskNo)) {
                    result.add(assign);
                }
            }
            return result;
        });
    }

    public void updateObjects(List<Object> objects, StdProcessContext context) {
        processRuntimeManager.op(context, () -> {
            ProcessCache cache = context.getCache();
            for (Object o : objects) {
                if (o instanceof ProcessTaskDTO) {
                    cache.put(((ProcessTaskDTO) o).getTaskNo(), o, true);
                } else if (o instanceof ProcessTaskAssignDTO) {
                    cache.put(((ProcessTaskAssignDTO) o).getAssignNo(), o, true);
                } else if (o instanceof ProcessTaskEventDTO) {
                    cache.put(((ProcessTaskEventDTO) o).getEventNo(), o, true);
                } else {
                    throw new UnsupportedOperationException("Unsupported operation type:" + o.getClass());
                }
            }
            return null;
        });
    }

    private ProcessTaskExport getProcessTaskExport() {
        if (processTaskExport == null) {
            processTaskExport = ObjectFactorys.getDefault().getObject(ProcessTaskExport.class);
        }
        return processTaskExport;
    }

    public ProcessRuntimeManager getProcessRuntimeManager() {
        return processRuntimeManager;
    }

    public void setProcessRuntimeManager(ProcessRuntimeManager processRuntimeManager) {
        this.processRuntimeManager = processRuntimeManager;
    }

}
