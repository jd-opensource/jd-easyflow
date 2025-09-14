package com.jd.easyflow.process.client.task.flow;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowStringUtil;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessTaskExport;
import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeManager;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcessContext;
import com.jd.easyflow.process.client.task.TaskConstants;
import com.jd.easyflow.process.client.task.service.TaskClientService;
import com.jd.easyflow.process.client.task.service.dto.TaskCreateParam;
import com.jd.easyflow.process.client.task.service.dto.TaskExecuteParam;

/**
 * @author liyuliang5
 *
 */
public abstract class BaseTaskNodeAction implements NodeAction {
    
    private static final Logger log = LoggerFactory.getLogger(BaseTaskNodeAction.class);

    private ProcessTaskExport processTaskExport;

    private ProcessRuntimeManager processRuntimeManager;

    private TaskClientService taskClientService;

    public <T> T createTask(NodeContext nodeContext, FlowContext context) {
        FlowParam param = context.getParam();
        Map<String, Object> paramMap = (Map<String, Object>) param.getParam();
        if (paramMap == null) {
            paramMap = new HashMap<>();
            context.getParam().setParam(paramMap);
        }
        FlowNode flowNode = context.getFlow().getNode(nodeContext.getNodeId());
        String instanceBizStatus = context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_STATUS);
        String instanceBizData = context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_DATA);

        TaskCreateParam taskCreateParam = new TaskCreateParam();
        taskCreateParam.setBizNo(context.get(StdFlowProcessConstants.FLOW_CTX_BIZNO));
        taskCreateParam.setInstanceBizStatus(instanceBizStatus);
        taskCreateParam.setInstanceBizData(instanceBizData);
        Map dataMap = new HashMap();
        StdProcessContext processContext = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
        ProcessInstanceDTO processInstance = processRuntimeManager.getProcessInstance(processContext);
        dataMap.put("processInstanceData", processInstance);
        taskCreateParam.setElFunction(exp -> context.getElEvaluator().eval(exp, nodeContext, context, dataMap));
        taskCreateParam.setFlowTaskProperties(
                (Map<String, Object>) context.getFlow().getProperty(TaskConstants.PROCESS_PROP_TASK_KEY));
        taskCreateParam.setNodeContext(nodeContext.get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX));
        taskCreateParam.setProcessContext(context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX));
        taskCreateParam.setProcessType(context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_TYPE));
        taskCreateParam.setProductCode(context.get(StdFlowProcessConstants.FLOW_CTX_PRODUCT_CODE));
        taskCreateParam.setTaskBizCode(flowNode.getId());
        taskCreateParam.setTaskBizName(flowNode.getName());
        taskCreateParam.setTaskProperties((Map<String, Object>) flowNode.getProperty(TaskConstants.NODE_PROP_TASK_KEY));
        taskCreateParam.setUser(context.get(StdFlowProcessConstants.FLOW_CTX_USER));

        String taskNo = taskClientService.createTask(taskCreateParam);
        paramMap.put(TaskConstants.PARAM_TASK_TASKNO, taskNo);
        return null;
    }
    
    /**
     * 
     * @param <T>
     * @param nodeContext
     * @param context
     * @return
     */
    public <T> T executeTask(NodeContext nodeContext, FlowContext context) {
        FlowParam param = context.getParam();
        FlowNode flowNode = context.getFlow().getNode(nodeContext.getNodeId());
        Map<String, Object> taskProperties = (Map<String, Object>) flowNode.getProperty(TaskConstants.NODE_PROP_TASK_KEY);
        Map<String, Object> executeConf = taskProperties == null ? null : (Map<String, Object>) taskProperties.get("execute");
        Map<String, Object> executeParamConf = executeConf == null ? null : (Map<String, Object>) executeConf.get("param");
        TaskExecuteParam taskExecuteParam = new TaskExecuteParam();
        String instanceBizStatus = null;
        String instanceBizData = null;
        String executeBizResult = null;
        String user = null;
        String executeBizData = null;
        if (executeParamConf != null) {
            String executeBizResultExp = (String) executeParamConf.get("executeBizResult");
            String executeBizDataExp = (String) executeParamConf.get("executeBizData");
            String instanceBizStatusExp = (String) executeParamConf.get("instanceBizStatus");
            String instanceBizDataExp = (String) executeParamConf.get("instanceBizData");
            String userExp = (String) executeParamConf.get("user");
            Map<String, Object> dataMap = new HashMap<>();
            if (FlowStringUtil.isNotEmpty(executeBizResultExp)) {
                executeBizResult = context.getElEvaluator().eval(executeBizResultExp, nodeContext, context, dataMap);
            }
            if (FlowStringUtil.isNotEmpty(executeBizDataExp)) {
                executeBizData = context.getElEvaluator().eval(executeBizDataExp, nodeContext, context, dataMap);
            }       
            if (FlowStringUtil.isNotEmpty(instanceBizStatusExp)) {
                instanceBizStatus = context.getElEvaluator().eval(instanceBizStatusExp, nodeContext, context, dataMap);
            } 
            if (FlowStringUtil.isNotEmpty(instanceBizDataExp)) {
                instanceBizData = context.getElEvaluator().eval(instanceBizDataExp, nodeContext, context, dataMap);
            } 
            if (FlowStringUtil.isNotEmpty(userExp)) {
                user = context.getElEvaluator().eval(userExp, nodeContext, context, dataMap);
            }             
        } else {
            instanceBizStatus = context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_STATUS);
            instanceBizData = context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_DATA);
            executeBizResult = param.getParam(TaskConstants.PARAM_TASK_EXECUTE_RESULT);
            executeBizData = param.getParam(TaskConstants.PARAM_TASK_EXECUTE_DATA);
            user = context.get(StdFlowProcessConstants.FLOW_CTX_USER);
            taskExecuteParam.setCmdListStr(param.getParam(TaskConstants.PARAM_TASK_EXECUTE_CMDLIST_STR));
            taskExecuteParam.setGroup2List(param.getParam(TaskConstants.PARAM_TASK_GROUP2_LIST));
            taskExecuteParam.setGroupList(param.getParam(TaskConstants.PARAM_TASK_GROUP_LIST));
            taskExecuteParam.setOperation(param.getParam(TaskConstants.PARAM_TASK_OPERATION));
            taskExecuteParam.setTaskExtData(param.getParam(TaskConstants.PARAM_TASK_EXT_DATA));
        }
        taskExecuteParam.setExecuteBizResult(executeBizResult);
        taskExecuteParam.setExecuteBizData(executeBizData);
        taskExecuteParam.setUser(user);
        taskExecuteParam.setBizNo(context.get(StdFlowProcessConstants.FLOW_CTX_BIZNO));
        taskExecuteParam.setProcessType(context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_TYPE));
        taskExecuteParam.setInstanceBizData(instanceBizData);
        taskExecuteParam.setInstanceBizStatus(instanceBizStatus); 
        taskExecuteParam.setProcessContext(context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX));
        taskExecuteParam.setNodeContext(nodeContext.get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX));
        taskExecuteParam.setTaskBizCode(flowNode.getId());

        taskExecuteParam.setTaskNo(param.getParam(TaskConstants.PARAM_TASK_TASKNO));
        taskExecuteParam.setFlowTaskProperties(
                (Map<String, Object>) context.getFlow().getProperty(TaskConstants.PROCESS_PROP_TASK_KEY));
        taskExecuteParam
                .setTaskProperties((Map<String, Object>) flowNode.getProperty(TaskConstants.NODE_PROP_TASK_KEY));
        executeTask(nodeContext, context, taskExecuteParam, instanceBizStatus, instanceBizData);
        return (T) executeBizResult;
    }

    public void executeTask(NodeContext nodeContext, FlowContext context, TaskExecuteParam taskExecuteParam, String instanceBizStatus, String instanceBizData) {
        StdProcessContext processContext = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
        taskClientService.executeTask(taskExecuteParam);
        if (instanceBizStatus != null) {
            processRuntimeManager.op(processContext, () -> {
                ProcessInstanceDTO processInstance = processRuntimeManager.getProcessInstance(processContext);
                processInstance.setBizStatus(instanceBizStatus);
                processRuntimeManager.updateProcessInstance(processInstance, processContext);
                return null;
            });            
        }
        if (instanceBizData != null) {
            processRuntimeManager.op(processContext, () -> {
                ProcessInstanceDTO processInstance = processRuntimeManager.getProcessInstance(processContext);
                processInstance.setBizData(instanceBizData);
                processRuntimeManager.updateProcessInstance(processInstance, processContext);
                return null;
            });
        }
        StdNodeContext processNodeContext = nodeContext.get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX);
        List<ProcessTaskDTO> taskList = taskClientService.findTaskByNodeInstanceNo(
                processNodeContext.getNodeInstanceNo(),
                (Map<String, Object>) context.getFlow().getProperty(TaskConstants.PROCESS_PROP_TASK_KEY),
                processContext);
        int pendingTaskCount = 0;
        int finishTaskCount = 0;
        Map<String, Integer> bizResultMap = new HashMap<>();
        for (ProcessTaskDTO task : taskList) {
            if (ProcessTaskConstants.TASK_STATUS_FINISH.equals(task.getStatus())) {
                finishTaskCount++;
                String bizResult = task.getExecuteBizResult();
                if (FlowStringUtil.isNotEmpty(bizResult)) {
                    Integer count = bizResultMap.get(bizResult);
                    if (count == null) {
                        count = 1;
                    } else {
                        count++;
                    }
                    bizResultMap.put(bizResult, count);
                }
            } else if (ProcessTaskConstants.TASK_STATUS_PENDING.equals(task.getStatus())) {
                pendingTaskCount++;
            }
        }
        nodeContext.put("task.pendingCount", pendingTaskCount);
        nodeContext.put("task.finishCount", finishTaskCount);
        nodeContext.put("task.allFinish", pendingTaskCount == 0);
        nodeContext.put("task.bizResults", bizResultMap);
        nodeContext.put("task.all", taskList);
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

    public TaskClientService getTaskClientService() {
        return taskClientService;
    }

    public void setTaskClientService(TaskClientService taskClientService) {
        this.taskClientService = taskClientService;
    }

    public void setProcessTaskExport(ProcessTaskExport processTaskExport) {
        this.processTaskExport = processTaskExport;
    }

}
