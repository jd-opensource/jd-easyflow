package com.jd.easyflow.process.client.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Supplier;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessInstanceExport;
import com.jd.easyflow.process.adapter.export.ProcessTransactionExport;
import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.LockProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.PersistDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeExecutionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessNodeReqDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.StdProcessContextDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.StdProcessDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.UnlockProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnCommand;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnRes;
import com.jd.easyflow.process.adapter.export.dto.transaction.command.BatchUpdateTxnCommand;
import com.jd.easyflow.process.adapter.export.dto.transaction.command.InterruptTxnCommand;
import com.jd.easyflow.process.client.common.PropertiesUtil;
import com.jd.easyflow.process.client.runtime.core.ProcessException;
import com.jd.easyflow.process.client.util.Pair;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 * 
 */
public class ProcessRuntimeManager {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessRuntimeManager.class);


    private ProcessInstanceExport processInstanceExport;

    private ProcessTransactionExport processTransactionExport;

    private ObjectIdManager objectIdManager = ObjectIdManager.INSTANCE;

    /**
     * 
     * @param processType
     * @param bizNo
     * @param context
     * @return
     */
    public ProcessInstanceDTO getProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo,
            StdProcessContext context) {
        return op(context, () -> {
            ProcessCache cache = context.getCache();
            for (ProcessInstanceDTO dto : cache.objects(ProcessInstanceDTO.class)) {
                if (StringUtils.equals(dto.getProcessType(), dto.getProcessType())
                        && StringUtils.equals(dto.getBizNo(), bizNo)) {
                    return dto;
                }
            }
            QueryProcessInstanceReq queryReq = QueryProcessInstanceReq.builder().processType(processType).bizNo(bizNo)
                    .build();
            ExportRequest<QueryProcessInstanceReq> request = new ExportRequest<>(queryReq);
            ProcessInstanceDTO instance = ExportResponseUtil
                    .unwrap(getProcessInstanceExport().queryProcessInstanceByProcessTypeAndBizNo(request));
            if (instance != null) {
                cache.put(instance.getInstanceNo(), instance, false);
            }
            return instance;
        });
    }

    /**
     * 
     * @param processType
     * @param bizNo
     * @param context
     * @return
     */
    public ProcessInstanceDTO getActiveProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo,
            StdProcessContext context) {
        return op(context, () -> {
            ProcessCache cache = context.getCache();
            for (ProcessInstanceDTO dto : cache.objects(ProcessInstanceDTO.class)) {
                if (StringUtils.equals(dto.getProcessType(), dto.getProcessType())
                        && StringUtils.equals(dto.getBizNo(), bizNo)
                        && StdProcessConstants.STATUS_ACTIVE.equals(dto.getStatus())) {
                    return dto;
                }
            }
            QueryProcessInstanceReq queryReq = QueryProcessInstanceReq.builder().processType(processType).bizNo(bizNo)
                    .build();
            ExportRequest<QueryProcessInstanceReq> request = new ExportRequest<>(queryReq);
            ProcessInstanceDTO instance = ExportResponseUtil
                    .unwrap(getProcessInstanceExport().queryActiveProcessInstanceByProcessTypeAndBizNo(request));
            if (instance != null) {
                cache.put(instance.getInstanceNo(), instance, false);
            }
            return instance;
        });
    }

    public ProcessInstanceDTO createProcessInstance(ProcessInstanceDTO instanceInfo, StdProcessContext context) {
        ProcessInstanceDTO instance = new ProcessInstanceDTO();
        ProcessConverter.INSTANCE.copy(instanceInfo, instance);
        String instanceNo = instanceInfo.getInstanceNo();
        if (StringUtils.isBlank(instanceNo)) {
            instanceNo = objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_PROCESS);
        }
        instance.setInstanceNo(instanceNo);
        instance.setStartTime(new Date());
        instance.setStatus(StdProcessConstants.STATUS_ACTIVE);
        instance.setCreatedDate(new Date());
        context.getCache().put(instance.getInstanceNo(), instance, true);
        return instance;
    }

    public ProcessNodeInstanceDTO getNodeInstance(String nodeInstanceNo, StdProcessContext context) {
        return op(context, () -> {
            ProcessNodeInstanceDTO instance = context.getCache().get(ProcessNodeInstanceDTO.class, nodeInstanceNo);
            if (instance != null) {
                return instance;
            }
            instance = ExportResponseUtil
                    .unwrap(getProcessInstanceExport().queryNodeInstanceByNo(new ExportRequest<>(nodeInstanceNo)));
            context.getCache().put(instance.getNodeInstanceNo(), instance, false);
            return instance;
        });
    }

    public List<ProcessNodeInstanceDTO> getNodeInstances(List<String> nodeInstanceNoList, StdProcessContext context) {
        return op(context, () -> {
            Map<String, ProcessNodeInstanceDTO> resultMap = new HashMap<>();
            List<String> queryList = new ArrayList<>();
            for (String nodeInstanceNo : nodeInstanceNoList) {
                ProcessNodeInstanceDTO nodeInstance = context.getCache().get(ProcessNodeInstanceDTO.class,
                        nodeInstanceNo);
                if (nodeInstance != null) {
                    resultMap.put(nodeInstanceNo, nodeInstance);
                } else {
                    queryList.add(nodeInstanceNo);
                }
            }
            if (queryList.size() > 0) {
                List<ProcessNodeInstanceDTO> nodeInstanceList = ExportResponseUtil
                        .unwrap(getProcessInstanceExport().queryNodeInstanceByNos(new ExportRequest<>(queryList)));
                for (ProcessNodeInstanceDTO nodeInstance : nodeInstanceList) {
                    context.getCache().put(nodeInstance.getNodeInstanceNo(), context, false);
                    resultMap.put(nodeInstance.getNodeInstanceNo(), nodeInstance);
                }
            }
            List<ProcessNodeInstanceDTO> result = new ArrayList<>();
            for (String nodeInstanceNo : nodeInstanceNoList) {
                ProcessNodeInstanceDTO nodeInstance = resultMap.get(nodeInstanceNo);
                if (nodeInstance == null) {
                    throw new ProcessException("Node:" + nodeInstanceNo + " not exists");
                }
                result.add(nodeInstance);
            }
            return result;
        });
    }

    public ProcessInstanceDTO getProcessInstance(StdProcessContext context) {
        return op(context, () -> {
            ProcessInstanceDTO instance = context.getCache().get(ProcessInstanceDTO.class, context.getInstanceNo());
            if (instance != null) {
                return instance;
            }
            instance = ExportResponseUtil.unwrap(
                    getProcessInstanceExport().getProcessInstance(new ExportRequest<>(context.getInstanceNo())));
            if (instance != null) {
                context.getCache().put(instance.getInstanceNo(), instance, false);
            }
            return instance;
        });
    }

    /**
     * 
     * @param nodeId
     * @param context
     * @return
     */
    public ProcessNodeInstanceDTO getOpenNodeInstance(String nodeId, StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Query node:{} active instances", nodeId);
        }
        return op(context, () -> {
            String instanceNo = context.getInstanceNo();
            for (ProcessNodeInstanceDTO nodeInstance : context.getCache().objects(ProcessNodeInstanceDTO.class)) {
                if (StringUtils.equals(nodeInstance.getProcessInstanceNo(), instanceNo)
                        && StringUtils.equals(nodeId, nodeInstance.getNodeId())
                        && !StringUtils.equals(nodeInstance.getStatus(), StdProcessConstants.NODE_STATUS_CLOSE)
                        && !StringUtils.equals(nodeInstance.getStatus(), StdProcessConstants.NODE_STATUS_INVALID)) {
                    if (log.isDebugEnabled()) {
                        log.debug("cache return:{}", nodeInstance);
                    }
                    return nodeInstance;
                }
            }
            return null;
        });
    }

    public ProcessNodeInstanceDTO getOneNodeInstance(String nodeId, StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Query one instance of node:{}", nodeId);
        }
        return op(context, () -> {
            String instanceNo = context.getInstanceNo();
            for (ProcessNodeInstanceDTO nodeInstance : context.getCache().objects(ProcessNodeInstanceDTO.class)) {
                if (StringUtils.equals(nodeInstance.getProcessInstanceNo(), instanceNo)
                        && StringUtils.equals(nodeId, nodeInstance.getNodeId())) {
                    if (log.isDebugEnabled()) {
                        log.debug("Cache return:{}", nodeInstance);
                    }
                    return nodeInstance;
                }
            }
            QueryProcessNodeReqDTO queryProcessNodeReq = QueryProcessNodeReqDTO.builder().nodeId(nodeId)
                    .processInstanceNo(instanceNo).build();
            List<ProcessNodeInstanceDTO> nodeInstanceList = ExportResponseUtil
                    .unwrap(getProcessInstanceExport().findNodeInstances(new ExportRequest<>(queryProcessNodeReq)));
            if (log.isDebugEnabled()) {
                log.debug("Database return:{}", nodeInstanceList);
            }
            if (nodeInstanceList != null && ! nodeInstanceList.isEmpty()) {
                nodeInstanceList.forEach(
                        nodeInstance -> context.getCache().put(nodeInstance.getNodeInstanceNo(), nodeInstance, false));
                for (ProcessNodeInstanceDTO instance : nodeInstanceList) {
                    if (!StdProcessConstants.NODE_STATUS_INVALID.equals(instance.getStatus())) {
                        return instance;
                    }
                }
            }
            return null;
        });
    }

    public ProcessNodeInstanceDTO getOrCreateOpenNodeInstance(String nodeId, StdProcessContext context) {
        return op(context, () -> {
            ProcessNodeInstanceDTO nodeInstance = getOpenNodeInstance(nodeId, context);
            if (nodeInstance == null) {
                nodeInstance = createNodeInstance(nodeId, context);
                if (log.isInfoEnabled()) {
                    log.info("Create node instance,node id:{},node instance id:{}", nodeInstance.getNodeId(), nodeInstance.getNodeInstanceNo());
                } else if (log.isDebugEnabled()) {
                    log.debug("Create node instance:{}", nodeInstance);
                }
            }
            return nodeInstance;
        });
    }
    
    public Pair<ProcessNodeInstanceDTO, Boolean> getOrCreateOpenNodeInstanceWithCreateFlag(String nodeId, StdProcessContext context) {
        return op(context, () -> {
            ProcessNodeInstanceDTO nodeInstance = getOpenNodeInstance(nodeId, context);
            boolean create = false;
            if (nodeInstance == null) {
                create = true;
                nodeInstance = createNodeInstance(nodeId, context);
                if (log.isInfoEnabled()) {
                    log.info("Create node instance,node id:{},node instance id:{}", nodeInstance.getNodeId(), nodeInstance.getNodeInstanceNo());
                } else if (log.isDebugEnabled()) {
                    log.debug("Create node instance:{}", nodeInstance);
                }
            }
            return Pair.of(nodeInstance, create);
        });
    }

    /**
     * 
     * @param nodeInstance
     * @param context
     */
    public void updateNodeExtData(ProcessNodeInstanceDTO nodeInstance, Map<String, Object> extData,
            StdProcessContext context) {
        op(context, () -> {
            if (extData != null && extData.size() > 0) {
                String currentExtDataStr = nodeInstance.getExtData();
                Map<String, Object> currentExtData = JSON.parseObject(currentExtDataStr, Map.class);
                if (currentExtData == null) {
                    currentExtData = new HashMap<>();
                }
                for (Entry<String, Object> entry : extData.entrySet()) {
                    currentExtData.put(entry.getKey(), entry.getValue());
                }
                String newExtDataStr = JSON.toJSONString(currentExtData);
                if (!StringUtils.equals(currentExtDataStr, newExtDataStr)) {
                    nodeInstance.setExtData(newExtDataStr);
                    updateNodeInstance(nodeInstance, context);
                }
            }
            return null;
        });
    }

    /**
     * 
     * @param instance
     * @param context
     */
    public void updateNodeInstance(ProcessNodeInstanceDTO instance, StdProcessContext context) {
        op(context, () -> {
            context.getCache().put(instance.getNodeInstanceNo(), instance, true);
            return null;
        });
    }

    public void updateProcessInstance(ProcessInstanceDTO instance, StdProcessContext context) {
        op(context, () -> {
            context.getCache().put(instance.getInstanceNo(), instance, true);
            return null;
        });
    }

    public void flushProcess(StdProcessContext context) {
        op(context, () -> {
            Set<String> openNodeIds = findOpenNodeIds(context);
            String[] flushNodes = context.getProcessProperty(StdProcessConstants.PROP_FLUSH_NODES);
            if (flushNodes != null) {
                for (String openNode : openNodeIds) {
                    if (!ArrayUtils.contains(flushNodes, openNode)) {
                        log.info("Node:" + openNode + " not in persist node list, No persist");
                        return null;
                    }
                }
            }
            ProcessInstanceDTO processInstance = getProcessInstance(context);
            if (StringUtils.isEmpty(processInstance.getBizNo()) && StringUtils.isNotEmpty(context.getBizNo())) {
                processInstance.setBizNo(context.getBizNo());
                context.getCache().put(processInstance.getInstanceNo(), processInstance, true);
            }

            String currentNodeIds = StringUtils.join(openNodeIds, ",");
            if (!StringUtils.equals(currentNodeIds, processInstance.getCurrentNodeIds())) {
                if (log.isDebugEnabled()) {
                    log.debug("Update process instance current node IDS:" + currentNodeIds);
                }
                processInstance.setCurrentNodeIds(currentNodeIds);

                Boolean end = context.getEnd();
                boolean processInstanceToEnd = false;
                if (end == null) {
                    end = false;
                    List<String> endNodeIds = context.getProcessProperty(StdProcessConstants.PROP_END_NODE_IDS);
                    if ((endNodeIds != null && !endNodeIds.isEmpty())
                            && (openNodeIds != null && !openNodeIds.isEmpty())) {
                        end = true;
                        for (String activeNodeId : openNodeIds) {
                            if (!endNodeIds.contains(activeNodeId)) {
                                end = false;
                                break;
                            }
                        }
                    }
                    if (end && StdProcessConstants.STATUS_ACTIVE.equals(processInstance.getStatus())) {
                        processInstance.setStatus(StdProcessConstants.STATUS_CLOSE);
                        processInstance.setEndTime(new Date());
                        processInstanceToEnd = true;
                    }
                }
                context.getCache().put(processInstance.getInstanceNo(), processInstance, true);
                if (processInstanceToEnd) {
                context.getEventTriggerFunction().apply(
                        new Object[] { StdProcessConstants.EVENT_PROCESS_INSTANCE_END, processInstance });
                }
            }
            flushTxn(context);
            return null;
        });
    }

    public Set<String> findActiveNodeIds(StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Query active node");
        }
        return op(context, () -> {
            String instanceNo = context.getInstanceNo();
            Set<String> status = new HashSet<>();
            status.add(StdProcessConstants.STATUS_ACTIVE);
            QueryProcessNodeReqDTO queryProcessNodeReq = QueryProcessNodeReqDTO.builder().processInstanceNo(instanceNo)
                    .status(status).build();
            List<ProcessNodeInstanceDTO> activeNodeInstances = ExportResponseUtil
                    .unwrap(getProcessInstanceExport().findNodeInstances(new ExportRequest<>(queryProcessNodeReq)));
            for (ProcessNodeInstanceDTO activeNodeInstance : activeNodeInstances) {
                if (context.getCache().get(ProcessNodeInstanceDTO.class,
                        activeNodeInstance.getNodeInstanceNo()) == null) {
                    context.getCache().put(activeNodeInstance.getNodeInstanceNo(), activeNodeInstance, false);
                }
            }
            Set<String> activeNodeIds = new HashSet<String>();
            for (ProcessNodeInstanceDTO nodeInstance : context.getCache().objects(ProcessNodeInstanceDTO.class)) {
                if (StringUtils.equals(instanceNo, nodeInstance.getProcessInstanceNo())
                        && StdProcessConstants.STATUS_ACTIVE.equals(nodeInstance.getStatus())) {
                    activeNodeIds.add(nodeInstance.getNodeId());
                }
            }
            if (log.isDebugEnabled()) {
                log.debug("Active node is" + activeNodeIds);
            }
            return activeNodeIds;
        });
    }

    public Set<String> loadOpenNodes(StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Query OPEN node");
        }
        return op(context, () -> {
            String instanceNo = context.getInstanceNo();
            Set<String> status = new HashSet<>(
                    Arrays.asList(StdProcessConstants.NODE_STATUS_ACTIVE, StdProcessConstants.NODE_STATUS_INACTIVE));
            QueryProcessNodeReqDTO queryProcessNodeReq = QueryProcessNodeReqDTO.builder().processInstanceNo(instanceNo)
                    .status(status).build();
            List<ProcessNodeInstanceDTO> openNodeInstances = ExportResponseUtil
                    .unwrap(getProcessInstanceExport().findNodeInstances(new ExportRequest<>(queryProcessNodeReq)));
            for (ProcessNodeInstanceDTO openNodeInstance : openNodeInstances) {
                if (context.getCache().get(ProcessNodeInstanceDTO.class,
                        openNodeInstance.getNodeInstanceNo()) == null) {
                    context.getCache().put(openNodeInstance.getNodeInstanceNo(), openNodeInstance, false);
                }
            }
            Set<String> openNodeIds = new HashSet<String>();
            for (ProcessNodeInstanceDTO nodeInstance : context.getCache().objects(ProcessNodeInstanceDTO.class)) {
                if (StringUtils.equals(instanceNo, nodeInstance.getProcessInstanceNo())
                        && (StdProcessConstants.NODE_STATUS_ACTIVE.equals(nodeInstance.getStatus())
                                || StdProcessConstants.NODE_STATUS_INACTIVE.equals(nodeInstance.getStatus()))) {
                    openNodeIds.add(nodeInstance.getNodeId());
                }
            }
            if (log.isDebugEnabled()) {
                log.debug("OPEN node is " + openNodeIds);
            }
            return openNodeIds;
        });
    }

    public Set<String> findOpenNodeIds(StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Query OPEN node in cache");
        }
        return op(context, () -> {
            String instanceNo = context.getInstanceNo();
            Set<String> openNodeIds = new HashSet<String>();
            for (ProcessNodeInstanceDTO nodeInstance : context.getCache().objects(ProcessNodeInstanceDTO.class)) {
                if (StringUtils.equals(instanceNo, nodeInstance.getProcessInstanceNo())
                        && (StdProcessConstants.NODE_STATUS_ACTIVE.equals(nodeInstance.getStatus())
                                || StdProcessConstants.NODE_STATUS_INACTIVE.equals(nodeInstance.getStatus()))) {
                    openNodeIds.add(nodeInstance.getNodeId());
                }
            }
            if (log.isDebugEnabled()) {
                log.debug("Open node in cache is " + openNodeIds);
            }
            return openNodeIds;
        });
    }
    
    public List<ProcessNodeInstanceDTO> findOpenNodeInstances(StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Query OPEN node instances in cache");
        }
        return op(context, () -> {
            List<ProcessNodeInstanceDTO> openNodeInstances = new ArrayList<ProcessNodeInstanceDTO>();
            String instanceNo = context.getInstanceNo();
            for (ProcessNodeInstanceDTO nodeInstance : context.getCache().objects(ProcessNodeInstanceDTO.class)) {
                if (StringUtils.equals(instanceNo, nodeInstance.getProcessInstanceNo())
                        && (StdProcessConstants.NODE_STATUS_ACTIVE.equals(nodeInstance.getStatus())
                                || StdProcessConstants.NODE_STATUS_INACTIVE.equals(nodeInstance.getStatus()))) {
                    openNodeInstances.add(nodeInstance);
                }
            }
            if (log.isDebugEnabled()) {
                log.debug("Open node instances in cache are " + openNodeInstances);
            }
            return openNodeInstances;
        });
    }
    
    /**
     * 
     * @param context
     */
    private void flushTxn(StdProcessContext context) {
        op(context, () -> {
            beforeFlushTxn(context);
            if (context.isSubProcess() && Boolean.TRUE.equals(PropertiesUtil
                    .get(context.getProcess().getProcessProperties(), StdProcessConstants.PROP_DATA_FLUSH_BY_PARENT))) {
                if (log.isDebugEnabled()) {
                    log.debug("Sub flow not flush data");
                }
                return null;
            }
            if (context.getCache().getCommandList().size() > 0) {
                TxnReq txnReq = new TxnReq();
                txnReq.setCommandList(context.getCache().getCommandList());
                context.getEventTriggerFunction().apply(new Object[] { StdProcessConstants.EVENT_TXN_FLUSH_START, new Object[] {txnReq}});
                TxnRes txnRes = ExportResponseUtil
                        .unwrap(getProcessTransactionExport().doTransaction(new ExportRequest<TxnReq>(txnReq)));
                context.getEventTriggerFunction().apply(new Object[] { StdProcessConstants.EVENT_TXN_FLUSH_END, new Object[] {txnReq, txnRes}});
                context.getCache().clearTxnCommand();
            }
            return null;
        });
    }

    private void beforeFlushTxn(StdProcessContext context) {
        List<Object> list = new ArrayList<Object>();
        for (Map<Object, CacheObject<?>> map : context.getCache().getCacheObjectMap().values()) {
            for (CacheObject<?> o : map.values()) {
                if (o.isDirty()) {
                    PersistDTO persistDto = new PersistDTO();
                    persistDto.setPersistObject(o.getObject());
                    persistDto.setPersistOp(o.getPersistOp());
                    list.add(persistDto);
                    o.setDirty(false);
                    o.setPersistOp(ProcessTransactionConstants.PERSIST_OP_NONE);
                }
            }
        }
        if (log.isDebugEnabled()) {
            log.debug("Data to flush is :{}", list);
        }
        if (list.size() > 0) {
            StdProcessContextDTO stdProcessContext = new StdProcessContextDTO();
            stdProcessContext.setProcessProperties(context.getProcessProperties());
            StdProcessDTO stdProcess = new StdProcessDTO();
            stdProcess.setExtProperties(((StdProcess) context.getProcess()).getExtProperties());
            stdProcess.setProcessProperties(((StdProcess) context.getProcess()).getProcessProperties());
            stdProcessContext.setProcess(stdProcess);
            BatchUpdateTxnCommand batchUpdateTxnCommand = new BatchUpdateTxnCommand();
            batchUpdateTxnCommand.setObjects(list);
            batchUpdateTxnCommand.setProcessContext(stdProcessContext);
            context.getCache().addTxnCommand(batchUpdateTxnCommand);
        }
        if (context.isInterrupted()) {
            log.info("Process instance interrupt");
            InterruptTxnCommand command = new InterruptTxnCommand();
            StdProcessContextDTO stdProcessContext = new StdProcessContextDTO();
            stdProcessContext.setProcessProperties(context.getProcessProperties());
            command.setProcessInstanceNo(context.getInstanceNo());
            command.setProcessContext(stdProcessContext);
            command.setInterruptTime(new Date());
            command.setInterruptUser(null);
            context.getCache().addTxnCommand(command);
        }
    }

    /**
     * 
     * @return
     */
    private ProcessNodeInstanceDTO createNodeInstance(String nodeId, StdProcessContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Create node instance:{}", nodeId);
        }
        ProcessNodeInstanceDTO nodeInstance = new ProcessNodeInstanceDTO();
        ProcessInstanceDTO instance = getProcessInstance(context);
        nodeInstance.setNodeId(nodeId);
        nodeInstance.setNodeInstanceNo(objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_NODE));
        nodeInstance.setProcessDefId(instance.getProcessDefId());
        nodeInstance.setProcessInstanceNo(instance.getInstanceNo());
        nodeInstance.setProductCode(instance.getProductCode());
        nodeInstance.setStartTime(new Date());
        nodeInstance.setStatus(StdProcessConstants.NODE_STATUS_INACTIVE);
        nodeInstance.setCreatedDate(new Date());
        context.getCache().put(nodeInstance.getNodeInstanceNo(), nodeInstance, true);
        return nodeInstance;
    }

    /**
     * 
     * @param nodeContext
     * @param context
     * @return
     */
    public ProcessNodeExecutionDTO createNodeExecution(StdNodeContext nodeContext, StdProcessContext context) {
        return op(context, () -> {
            ProcessNodeExecutionDTO execution = new ProcessNodeExecutionDTO();
            execution.setEndTime(new Date());
            execution.setEventId(nodeContext.getEventId());
            execution.setNextNodeInstances(StringUtils.join(nodeContext.getNextNodeInstanceNos(), ","));
            execution.setNodeExecutionNo(objectIdManager.nextObjectId(ProcessTransactionConstants.TYPE_EXECUTION));
            execution.setNodeId(nodeContext.getNodeId());
            ProcessNodeInstanceDTO nodeInstance = getNodeInstance(nodeContext.getNodeInstanceNo(), context);
            execution.setNodeInstanceNo(nodeInstance.getNodeInstanceNo());
            execution.setProcessDefId(nodeInstance.getProcessDefId());
            execution.setProductCode(nodeInstance.getProductCode());
            execution.setStartTime(nodeContext.getExecutionStartTime());
            execution.setStatus(StdProcessConstants.EXECUTION_STATUS_CLOSE);
            execution.setCreatedDate(new Date());
            context.getCache().put(execution.getNodeExecutionNo(), execution, true);
            return execution;
        });
    }

    public <T> T op(StdProcessContext context, Supplier<T> supplier) {
        synchronized (context.getLock()) {
            return (T) supplier.get();
        }
    }

    public String lockProcessInstance(String processType, String bizNo) {
        LockProcessInstanceReq req = new LockProcessInstanceReq();
        req.setProcessType(processType);
        req.setBizNo(bizNo);
        return ExportResponseUtil
                .unwrap(getProcessInstanceExport().lockProcessInstance(new ExportRequest<LockProcessInstanceReq>(req)));
    }

    public Boolean unLockProcessInstance(String processType, String bizNo, String lockRequestId) {
        UnlockProcessInstanceReq req = new UnlockProcessInstanceReq();
        req.setProcessType(processType);
        req.setBizNo(bizNo);
        req.setLockRequestId(lockRequestId);
        return ExportResponseUtil.unwrap(
                getProcessInstanceExport().unLockProcessInstance(new ExportRequest<UnlockProcessInstanceReq>(req)));
    }

    public void addTxn(TxnCommand txnCommand, StdProcessContext context) {
        op(context, () -> {
            context.getCache().addTxnCommand(txnCommand);
            return null;
        });
    }

    private ProcessInstanceExport getProcessInstanceExport() {
        if (processInstanceExport != null) {
            return processInstanceExport;
        }
        processInstanceExport = ObjectFactorys.getDefault().getObject(ProcessInstanceExport.class);
        return processInstanceExport;
    }

    private ProcessTransactionExport getProcessTransactionExport() {
        if (processTransactionExport != null) {
            return processTransactionExport;
        }
        processTransactionExport = ObjectFactorys.getDefault().getObject(ProcessTransactionExport.class);
        return processTransactionExport;
    }

    public ObjectIdManager getObjectIdManager() {
        return objectIdManager;
    }

    public void setObjectIdManager(ObjectIdManager objectIdManager) {
        this.objectIdManager = objectIdManager;
    }

}
