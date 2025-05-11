package com.jd.easyflow.process.client.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeExecutionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.client.common.PropertiesUtil;
import com.jd.easyflow.process.client.runtime.core.ProcessException;
import com.jd.easyflow.process.client.util.Pair;
import com.jd.easyflow.process.spi.client.enums.ProcessClientResponseCode;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 * 
 */
public class ProcessRuntimeService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessRuntimeService.class);


    private ProcessRuntimeManager manager;

    public void processStartExec(ProcessInstanceDTO instance, StdProcessContext context) {
        log.info("Start process execute, processType:{}, bizNo:{}", instance.getProcessType(), instance.getBizNo());

        ProcessInstanceDTO processInstance = null;
        Map<String, Object> processProperties = ((StdProcess) context.getProcess()).getProcessProperties();
        if (!Boolean.TRUE
                .equals(PropertiesUtil.get(processProperties, StdProcessConstants.PROP_CREATE_INSTANCE_EVERY_TIME))) {
            String bizRelation = PropertiesUtil.get(processProperties, StdProcessConstants.PROP_BIZ_RELATION);
            if (StringUtils.isEmpty(bizRelation) || StdProcessConstants.BIZ_RELATION_ONE_ONE.equals(bizRelation)) {
                processInstance = manager.getProcessInstanceByProcessTypeAndBizNo(instance.getProcessType(),
                        instance.getBizNo(), context);

                if (processInstance != null
                        && StdProcessConstants.STATUS_CANCELED.equals(processInstance.getStatus())) {
                    Map<String, Object> data = new HashMap<>();
                    data.put("instanceNo", context.getInstanceNo());
                    ProcessException exception = new ProcessException(ProcessRuntimeErrorCode.PR_0104.name(),
                            ProcessRuntimeErrorCode.PR_0104.getDesc());
                    exception.setData(data);
                    throw exception;
                }

            } else if (StdProcessConstants.BIZ_RELATION_MANY_ONE.equals(bizRelation)) {
                processInstance = manager.getActiveProcessInstanceByProcessTypeAndBizNo(instance.getProcessType(),
                        instance.getBizNo(), context);
            } else {
                throw new IllegalArgumentException("Illegal bizRelation:" + bizRelation);
            }

            if (StdProcessConstants.OP_TYPE_CREATE.equals(context.getOpType())) {
                if (processInstance != null) {
                    throw new ProcessException(ProcessClientResponseCode.PROCESS_INSTANCE_EXISTS.getCode(),
                            "Process instance exists:" + processInstance.getInstanceNo());
                }

            } else if (StdProcessConstants.OP_TYPE_EXECUTE.equals(context.getOpType())) {
                if (processInstance == null) {
                    throw new ProcessException(
                            "Process instance not exists, processType:" + instance.getProcessType() + " bizNo:" + instance.getBizNo());
                }
            }

            if (processInstance != null && StdProcessConstants.STATUS_CLOSE.equals(processInstance.getStatus())) {
                String exeucteClosePoliy = PropertiesUtil.get(processProperties,
                        StdProcessConstants.PROP_EXECUTE_CLOSE_POLICY);

                if (StdProcessConstants.POLICY_EXCEPTION.equals(exeucteClosePoliy)) {
                    Map<String, Object> data = new HashMap<>();
                    data.put("instanceNo", context.getInstanceNo());
                    ProcessException exception = new ProcessException(ProcessRuntimeErrorCode.PR_0103.name(),
                            ProcessRuntimeErrorCode.PR_0103.getDesc());
                    exception.setData(data);
                    throw exception;
                }
            }
        }

        List<String> startNodeIds = new ArrayList<>();
        if (processInstance != null) {
            context.setInstanceNo(processInstance.getInstanceNo());
            manager.loadOpenNodes(context);
        }
        startNodeIds = context.getStartNodesFunction().apply(processInstance);
        context.setStartNodeIds(startNodeIds);
        if (processInstance == null) {
            processInstance = tryCreateProcessInstance(instance, context);
            context.setInstanceNo(processInstance.getInstanceNo());
            if (Boolean.TRUE
                    .equals(PropertiesUtil.get(processProperties, StdProcessConstants.PROP_DATA_FLUSH_AFTER_CREATE))) {
                syncVariable(context);
                manager.flushProcess(context);
            }
        }

        boolean update = false;
        if (!StringUtils.equals(processInstance.getProcessDefId(), instance.getProcessDefId())) {
            processInstance.setProcessDefId(instance.getProcessDefId());
            update = true;
        }
        if (instance.getBizStatus() != null
                && !StringUtils.equals(processInstance.getBizStatus(), instance.getBizStatus())) {
            processInstance.setBizStatus(instance.getBizStatus());
            update = true;
        }
        if (instance.getBizData() != null && !StringUtils.equals(processInstance.getBizData(), instance.getBizData())) {
            processInstance.setBizData(instance.getBizData());
            update = true;
        }
        if (update) {
            manager.updateProcessInstance(processInstance, context);
        }

        checkStartNodes(context);

        String variablesStr = processInstance.getVars();
        if (StringUtils.isNotEmpty(variablesStr)) {
            Map<String, String> variables = JSON.parseObject(variablesStr, Map.class);
            context.getVariableSetter().accept(variables);
        }
    }

    public void nodeStartExec(StdNodeContext nodeContext, StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Node start exec, nodeContext:{}", nodeContext);
        }
        manager.op(context, () -> {
            ProcessNodeInstanceDTO nodeInstance;
            boolean nodeNewCreate;
            if (FlowConstants.NODE_PRE_CHECK_TYPE_MULTICHECK.equals(nodeContext.getPreCheckType()) || FlowConstants.NODE_PRE_CHECK_TYPE_INCLUSIVECHECK.equals(nodeContext.getPreCheckType())) {
                // can only be created on previous node end.
                nodeInstance = manager.getOpenNodeInstance(nodeContext.getNodeId(), context);
                if (nodeInstance == null) {
                    log.info("Open node instance is null," + nodeContext );
                    nodeContext.setPreResult(false);
                    return null;
                }
                nodeNewCreate = false;
            } else {
                Pair<ProcessNodeInstanceDTO, Boolean> nodeInstanceInfo = manager.getOrCreateOpenNodeInstanceWithCreateFlag(nodeContext.getNodeId(), context);
                nodeInstance = nodeInstanceInfo.getLeft();
                nodeNewCreate = nodeInstanceInfo.getRight();
            }
            
            if (StringUtils.isNotEmpty(nodeInstance.getVars())) {
                context.getNodeVariableSetter().accept(Pair.of(nodeContext, JSON.parseObject(nodeInstance.getVars(), Map.class)));
            }
            if (nodeNewCreate && context.getNodeStartEventPolicy() == StdProcessConstants.NODE_START_EVENT_POLICY_CREATE) {
                context.getEventTriggerFunction().apply(new Object[] { StdProcessConstants.EVENT_NODE_INSTANCE_START, new Object[] {nodeInstance, nodeContext} });
            }
            if (FlowConstants.NODE_PRE_CHECK_TYPE_MULTICHECK.equals(nodeContext.getPreCheckType())) {
                if (StdProcessConstants.NODE_STATUS_ACTIVE.equals(nodeInstance.getStatus())) {
                    log.info("Node " + nodeContext + " finish precheck");
                    nodeContext.setPreResult(false);
                } else if (StdProcessConstants.NODE_STATUS_INACTIVE.equals(nodeInstance.getStatus())) {
                    boolean preResult = false;
                    String previousNodeInstanceNos = nodeInstance.getPreviousNodeInstances();
                    List<String> previousNodeInstanceNoList = Arrays.asList(previousNodeInstanceNos.split(","));
                    List<ProcessNodeInstanceDTO> previousNodeInstanceList = manager
                            .getNodeInstances(previousNodeInstanceNoList, context);
                    Set<String> previousNodeIds = new HashSet<>();
                    previousNodeInstanceList.forEach(element -> previousNodeIds.add(element.getNodeId()));
                    boolean active = true;
                    if (nodeContext.getConfigPreNodeIds() != null && ! nodeContext.getConfigPreNodeIds().isEmpty()) {
                        for (String confNodeId : nodeContext.getConfigPreNodeIds()) {
                            if (!previousNodeIds.contains(confNodeId)) {
                                active = false;
                                break;
                            }
                        }
                    }
                    if (active) {
                        log.info(nodeInstance.getNodeInstanceNo() + " " + nodeInstance.getNodeId() + " node is active.");
                        nodeInstance.setStatus(StdProcessConstants.NODE_STATUS_ACTIVE);
                        manager.updateNodeInstance(nodeInstance, context);
                        if (context.getNodeStartEventPolicy() == StdProcessConstants.NODE_START_EVENT_POLICY_ACTIVE) {
                            nodeInstance.setStartTime(new Date());
                            context.getEventTriggerFunction().apply(new Object[] { StdProcessConstants.EVENT_NODE_INSTANCE_START, new Object[] {nodeInstance, nodeContext} });
                        }
                        preResult = true;
                    }
                    nodeContext.setPreResult(preResult);
                } else {
                    throw new IllegalStateException("Illegal node status:" + nodeInstance);
                }
                
            } else if (FlowConstants.NODE_PRE_CHECK_TYPE_INCLUSIVECHECK.equals(nodeContext.getPreCheckType())) {
                ProcessInclusiveCheckHelper.nodeStartExec(nodeContext, context, nodeInstance, manager);
            } else {
                if (StdProcessConstants.NODE_STATUS_INACTIVE.equals(nodeInstance.getStatus())) {
                    nodeInstance.setStatus(StdProcessConstants.NODE_STATUS_ACTIVE);
                    manager.updateNodeInstance(nodeInstance, context);
                    if (context.getNodeStartEventPolicy() == StdProcessConstants.NODE_START_EVENT_POLICY_ACTIVE) {
                        nodeInstance.setStartTime(new Date());
                        context.getEventTriggerFunction().apply(new Object[] { StdProcessConstants.EVENT_NODE_INSTANCE_START, new Object[] {nodeInstance, nodeContext}});
                    }
                }
            }
            manager.updateNodeExtData(nodeInstance, nodeContext.getExtData(), context);
            nodeContext.setNodeInstanceNo(nodeInstance.getNodeInstanceNo());
            
            String dataFlushPolicy = PropertiesUtil.getProperty(StdProcessConstants.PROP_DATA_FLUSH_POLICY, nodeContext,
                    context);
            if (StdProcessConstants.FLUSH_BEFORE_NODE.equals(dataFlushPolicy)
                    || StdProcessConstants.FLUSH_BEFORE_AND_AFTER_NODE.equals(dataFlushPolicy)) {
                syncVariable(context);
                manager.flushProcess(context);
            }
            return null;
        });
    }

    public void nodeEndExec(StdNodeContext nodeContext, StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Node end exec, nodeContext:{}", nodeContext);
        }

        manager.op(context, () -> {
            if (nodeContext.getNodeInstanceNo() == null) {
                log.info("node instance no is null, " + nodeContext);
                return null;
            }
            ProcessNodeInstanceDTO instance = manager.getNodeInstance(nodeContext.getNodeInstanceNo(), context);

            Map<String, String> nodeVariables = context.getNodeVariableGetter().apply(nodeContext);
            String currentVariableStr = instance.getVars();
            String newVariablesStr = null;
            if (!(StringUtils.isEmpty(currentVariableStr) && (nodeVariables == null || nodeVariables.isEmpty()))) {
                newVariablesStr = JSON.toJSONString(nodeVariables);
                if (!StringUtils.equals(newVariablesStr, currentVariableStr)) {
                    instance.setVars(newVariablesStr);
                    manager.updateNodeInstance(instance, context);
                }
            }
            
            
            if (nodeContext.getNextNodeIds() != null) {

                List<String> nextNodeInstanceNos = new ArrayList<>();
                List<ProcessNodeInstanceDTO> createdNextNodeList = new ArrayList<>();
                for (String nodeId : nodeContext.getNextNodeIds()) {
                    Pair<ProcessNodeInstanceDTO, Boolean> nodeInstanceInfo = manager
                            .getOrCreateOpenNodeInstanceWithCreateFlag(nodeId, context);
                    ProcessNodeInstanceDTO nodeInstance = nodeInstanceInfo.getLeft();
                    if (nodeInstanceInfo.getRight()) {
                        createdNextNodeList.add(nodeInstance);
                    }
                    nextNodeInstanceNos.add(nodeInstance.getNodeInstanceNo());

                    String savePreviousPolicy = PropertiesUtil.get(
                            ((StdNode) nodeContext.getNode()).getProcessProperties(),
                            StdProcessConstants.PROP_SAVE_PREVIOUS_POLICY);
                    if (!StdProcessConstants.SAVE_PREVIOUS_POLICY_NONE.equals(savePreviousPolicy)) {
                        String previousStr = nodeInstance.getPreviousNodeInstances();
                        if (StringUtils.isEmpty(previousStr)) {
                            previousStr = nodeContext.getNodeInstanceNo();
                            nodeInstance.setPreviousNodeInstances(previousStr);
                            manager.updateNodeInstance(nodeInstance, context);
                        } else {
                            String[] previousNodeInstanceNos = StringUtils.split(previousStr, ",");
                            if (!ArrayUtils.contains(previousNodeInstanceNos, nodeContext.getNodeInstanceNo())) {
                                previousStr += "," + nodeContext.getNodeInstanceNo();
                                nodeInstance.setPreviousNodeInstances(previousStr);
                                manager.updateNodeInstance(nodeInstance, context);
                            }
                        }
                    }
                }

                nodeContext.setNextNodeInstanceNos(nextNodeInstanceNos.toArray(new String[] {}));

                instance.setEndTime(new Date());
                instance.setNextNodeInstances(StringUtils.join(nextNodeInstanceNos, ","));
                instance.setStatus(StdProcessConstants.NODE_STATUS_CLOSE);
                manager.updateNodeInstance(instance, context);
                context.getEventTriggerFunction().apply(new Object[] { StdProcessConstants.EVENT_NODE_INSTANCE_END,
                        new Object[] { instance, nodeContext } });
                if (context.getNodeStartEventPolicy() == StdProcessConstants.NODE_START_EVENT_POLICY_CREATE) {
                    for (ProcessNodeInstanceDTO nextNodeInstance : createdNextNodeList) {
                        context.getEventTriggerFunction()
                                .apply(new Object[] { StdProcessConstants.EVENT_NODE_INSTANCE_START,
                                        new Object[] { nextNodeInstance, nodeContext } });
                    }
                }

                ProcessNodeExecutionDTO execution = manager.createNodeExecution(nodeContext, context);
                if (log.isDebugEnabled()) {
                    log.debug("Create node execution:{}", execution);
                }
            }
            ProcessInclusiveCheckHelper.nodeEndExec(nodeContext, context, manager);

            String dataFlushPolicy = PropertiesUtil.getProperty(StdProcessConstants.PROP_DATA_FLUSH_POLICY, nodeContext,
                    context);
            if (StdProcessConstants.FLUSH_AFTER_NODE.equals(dataFlushPolicy)
                    || StdProcessConstants.FLUSH_BEFORE_AND_AFTER_NODE.equals(dataFlushPolicy)) {
                syncVariable(context);
                manager.flushProcess(context);
            }
            return null;
        });
    }

    public void processEndExec(StdProcessContext context) {
        log.info("process end exec");
        syncVariable(context);
        manager.flushProcess(context);
    }

    private void syncVariable(StdProcessContext context) {
        Map<String, String> variables = context.getVariableGetter().get();
        ProcessInstanceDTO instance = manager.getProcessInstance(context);
        String currentVariableStr = instance.getVars();
        if (StringUtils.isEmpty(currentVariableStr) && (variables == null || variables.isEmpty())) {
            return;
        }
        String newVariables = JSON.toJSONString(variables);
        if (!StringUtils.equals(instance.getVars(), newVariables)) {
            instance.setVars(newVariables);
            manager.updateProcessInstance(instance, context);
        }
    }

    public void processCompleteExec(StdProcessContext context) {
        if (ObjectUtils.isNotEmpty(context)) {
            manager.unLockProcessInstance(context.getProcessType(), context.getBizNo(), context.getLockRequestId());
        } else {
            log.warn("ProcessRuntimeService-processCompleteExec-stdProcessContext is null. please to check it");
        }
    }

    private ProcessInstanceDTO tryCreateProcessInstance(ProcessInstanceDTO instance, StdProcessContext context) {
        if (context.isCheckStartNode()) {
            Boolean start = context.getStart();
            if (start == null) {
                start = true;
                List<String> startNodeIds = context.getProcessProperty(StdProcessConstants.PROP_START_NODE_IDS);
                if ((startNodeIds != null && ! startNodeIds.isEmpty()) && (context.getStartNodeIds() != null && ! context.getStartNodeIds().isEmpty())) {
                    for (String startNodeId : context.getStartNodeIds()) {
                        if (!startNodeIds.contains(startNodeId)) {
                            start = false;
                        }
                    }
                }
            }
            if (!start) {
                log.info("Not start node, can not create process instance.");
                throw new ProcessException(
                        "Not start node, can not create process instance. Configured start node:" + context.getProcessProperty(StdProcessConstants.PROP_START_NODE_IDS)
                                + ", input start node:" + context.getStartNodeIds() + ", process instance:" + instance);
            }
        }
        ProcessInstanceDTO processInstance = manager.createProcessInstance(instance, context);
        context.getEventTriggerFunction()
        .apply(new Object[] { StdProcessConstants.EVENT_PROCESS_INSTANCE_START, instance });
        log.info("Create process instance:" + JSON.toJSONString(processInstance));
        return processInstance;
    }

    private void checkStartNodes(StdProcessContext context) {
        if (!context.isCheckStartNode()) {
            return;
        }
        List<String> startNodeIds = context.getStartNodeIds();
        List<String> configStartNodeIds = context.getProcessProperty(StdProcessConstants.PROP_START_NODE_IDS);
        Set<String> currentOpenNodeIds = manager.findOpenNodeIds(context);
        if (startNodeIds != null) {
            for (String startNodeId : startNodeIds) {
                if (currentOpenNodeIds.contains(startNodeId)) {
                    continue;
                }
                if (configStartNodeIds != null && configStartNodeIds.contains(startNodeId)) {
                    StdNode node = context.getNodeFunction().apply(startNodeId);
                    String startCheckPolicy = PropertiesUtil.get(node.getProcessProperties(),
                            StdProcessConstants.PROP_START_CHECK_POLICY);
                    if (StdProcessConstants.START_CHECK_POLICY_UNLIMIT.equals(startCheckPolicy)) {
                        continue;
                    }
                    ProcessNodeInstanceDTO instance = manager.getOneNodeInstance(startNodeId, context);
                    if (instance != null && StdProcessConstants.NODE_STATUS_CLOSE.equals(instance.getStatus())) {
                        Map<String, Object> data = new HashMap<>();
                        data.put("instanceNo", context.getInstanceNo());
                        ProcessException exception = new ProcessException(ProcessRuntimeErrorCode.PR_0101.name(),
                                "Start node close, can not start exec:" + startNodeId);
                        exception.setData(data);
                        throw exception;
                    } else {
                        continue;
                    }
                }
                Map<String, Object> data = new HashMap<>();
                data.put("instanceNo", context.getInstanceNo());
                ProcessException exception = new ProcessException(ProcessRuntimeErrorCode.PR_0102.name(),
                        "Not start node or open node, cannot start exec:" + startNodeId + " current active node:" + currentOpenNodeIds);
                exception.setData(data);
                throw exception;
            }
        }
    }

    public ProcessInstanceDTO getProcessInstance(StdProcessContext context) {
        return manager.getProcessInstance(context);
    }

    public String lockProcessInstance(String processType, String bizNo) {
        return manager.lockProcessInstance(processType, bizNo);
    }

    public ProcessRuntimeManager getManager() {
        return manager;
    }

    public void setManager(ProcessRuntimeManager manager) {
        this.manager = manager;
    }

}
