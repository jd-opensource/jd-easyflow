package com.jd.easyflow.process.domain.service;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.process.adapter.export.constant.ProcessInstanceConstants;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskAssignEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.vo.QueryProcessNodeReq;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.RollbackNodeReqVO;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.process.domain.repository.ProcessTaskRepository;

/**
 * @author liyuliang5
 */
public class ProcessToolDomainService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessToolDomainService.class);
    
    @Autowired
    private ProcessRepository processRepository;
    @Autowired
    private ProcessTaskRepository processTaskRepository;
    @Autowired
    private ProcessInstanceDomainService processInstanceDomainService;
    
    private TransactionTemplate transactionTemplate;

    public void rollbackNode(RollbackNodeReqVO req) {
        String processInstanceNo = req.getProcessInstanceNo();
        String targetNodeId = req.getTargetNodeId();
        String targetNodeInstanceNo = req.getTargetNodeInstanceNo();
        ProcessInstanceEntity processInstance = processRepository.getByProcessInstanceNo(processInstanceNo);
        AssertUtils.TIPS.isNotNull(processInstance, "instance " + processInstanceNo + " not exists");
        String lockProcessId = processInstanceDomainService.lockProcessInstance(processInstance.getProcessType(), processInstance.getBizNo());
        try {
            transactionTemplate.executeWithoutResult((status) -> {
                // query
                ProcessNodeInstanceEntity processNodeInstance = null;
                if (targetNodeId != null) {
                    QueryProcessNodeReq query = new QueryProcessNodeReq();
                    query.setProcessInstanceNo(processInstanceNo);
                    query.setNodeId(targetNodeId);
                    List<ProcessNodeInstanceEntity> nodeInstances = processRepository.findNodeInstances(query);
                    log.info("nodeInstances:" + nodeInstances);
                    if (nodeInstances.size() == 0) {
                        throw new UserException("nodeId:" + targetNodeId + " not exists in process instance " + processInstanceNo);
                    } 
                    if (targetNodeInstanceNo == null) {
                        if (nodeInstances.size() == 1) {
                            processNodeInstance = nodeInstances.get(0);
                        } else {
                            throw new UserException("nodeId:" + targetNodeId + "  has more than one node instances, please set targetNodeInstanceNo. " + nodeInstances);

                        }
                    } else {
                       for (ProcessNodeInstanceEntity entity : nodeInstances) {
                           if (entity.getNodeInstanceNo().equals(targetNodeInstanceNo)) {
                               processNodeInstance = entity;
                               break;
                           }
                       }
                       if (processNodeInstance == null) {
                           throw new UserException("nodeId:" + targetNodeId + "  has no targetNodeInstanceNo " + targetNodeInstanceNo);
                       }
                    }
                } else {
                    processNodeInstance = processRepository.getByNodeInstanceNo(targetNodeInstanceNo);
                    AssertUtils.TIPS.isNotNull(processNodeInstance, "node instance " + targetNodeInstanceNo + " not exists");
                }
                
                log.info("start rollback processInstance:[" + processInstance + "], nodeInstance:[" + processNodeInstance + "]");
                // check
                AssertUtils.isTrue(processInstanceNo.equals(processNodeInstance.getProcessInstanceNo()),
                        "process instance not same," + processNodeInstance.getProcessInstanceNo());
                if (!ProcessInstanceConstants.NODE_STATUS_CLOSE.equals(processNodeInstance.getStatus())) {
                    throw new UserException("Node instance status is not CLOSE, " + processNodeInstance.getNodeInstanceNo());
                }
                if (ProcessInstanceConstants.STATUS_CANCELED.equals(processInstance.getStatus())) {
                    throw new UserException("Process instance status is CANCELED, " + processNodeInstance.getNodeInstanceNo());
                }
                
                
                
                String nextNodeInstancesStr = processNodeInstance.getNextNodeInstances();
                if (nextNodeInstancesStr != null && ! nextNodeInstancesStr.isEmpty()) {
                    String[] nextNodeInstanceNos = nextNodeInstancesStr.split(",");
                    for (String nextNodeInstanceNo : nextNodeInstanceNos) {
                        rollbackNodeLink(nextNodeInstanceNo, processNodeInstance.getNodeInstanceNo(), req);
                    }
                }
                
                // update target node instance.
                processNodeInstance.setNextNodeInstances(null);
                processNodeInstance.setStatus(ProcessInstanceConstants.NODE_STATUS_ACTIVE);
                log.info("process node instance " + processNodeInstance.getNodeInstanceNo() + " updated, [" + processNodeInstance + "]");
                processRepository.updateProcessNodeInstanceByNo(processNodeInstance);
                // update process instance.
                List<ProcessNodeInstanceEntity> processNodeInstances = processRepository.findOpenNodeInstances(processInstanceNo);
                Set<String> currentNodeIds = new HashSet<>();
                for (ProcessNodeInstanceEntity node : processNodeInstances) {
                    currentNodeIds.add(node.getNodeInstanceNo());
                }
                ProcessInstanceEntity currentProcessInstance = processRepository.getByProcessInstanceNo(processInstanceNo);
                currentProcessInstance.setStatus(ProcessInstanceConstants.STATUS_ACTIVE);
                currentProcessInstance.setCurrentNodeIds(String.join(",", currentNodeIds));
                log.info("process instance " + processInstanceNo + " updated,[" + currentProcessInstance);
                processRepository.updateProcessInstanceByNo(currentProcessInstance);
            });
        } finally {
            processInstanceDomainService.unLockProcessInstance(processInstance.getProcessType(), processInstance.getBizNo(), lockProcessId);
        }
    }
    
    private void rollbackNodeLink(String nodeInstanceNo, String previousNodeInstanceNo, RollbackNodeReqVO req) {
        ProcessNodeInstanceEntity processNodeInstance = processRepository
                .getByNodeInstanceNo(nodeInstanceNo);
        if (processNodeInstance == null) {
            log.info("node:" + nodeInstanceNo + " not exists, may has been rollback");
            return;
        }
        log.info("start rollback node " + processNodeInstance.getNodeId() + ":"  + nodeInstanceNo + "[" + processNodeInstance + "]");
        String nextNodeInstancesStr = processNodeInstance.getNextNodeInstances();
        if (nextNodeInstancesStr != null && ! nextNodeInstancesStr.isEmpty()) {
            String[] nextNodeInstanceNos = nextNodeInstancesStr.split(",");
            for (String nextNodeInstanceNo : nextNodeInstanceNos) {
                rollbackNodeLink(nextNodeInstanceNo, nodeInstanceNo, req);
            }
        }
        log.info("start do rollback node " + processNodeInstance.getNodeId() + ":" + nodeInstanceNo);
        // rollback node
        String previousNodeInstanceNoStr = processNodeInstance.getPreviousNodeInstances();
        if (previousNodeInstanceNoStr.equals(previousNodeInstanceNo)) {
            log.info("node instance:" + nodeInstanceNo + " previous nodes is " + previousNodeInstanceNo + ", DELETE node instance,[" + processNodeInstance + "]");
            deleteNodeInstance(req.getProcessInstanceNo(), nodeInstanceNo, req.isRollbackSubProcess(), req.isRollbackTask());
        } else {
            String[] previousNodeInstanceNos = previousNodeInstanceNoStr.split(",");
            StringBuilder builder = new StringBuilder();
            for (String nodeInstance : previousNodeInstanceNos) {
                if (! nodeInstance.equals(previousNodeInstanceNo)) {
                    builder.append(nodeInstance).append(",");
                }
            }
            String newPreviousNodeInstanceNoStr = builder.substring(0, builder.length() - 1);
            processNodeInstance.setPreviousNodeInstances(newPreviousNodeInstanceNoStr);
            processNodeInstance.setStatus(ProcessInstanceConstants.NODE_STATUS_INACTIVE);
            processNodeInstance.setNextNodeInstances(null);
            log.info("node instance:" + nodeInstanceNo + " previousnodes is " + previousNodeInstanceNoStr + ", remove "
                    + previousNodeInstanceNo + ", left:" + newPreviousNodeInstanceNoStr + ", INVACTIVE node instance,[" + processNodeInstance + "]");
            processRepository.updateProcessNodeInstanceByNo(processNodeInstance);
            deleteTaskByNodeInstanceNo(nodeInstanceNo);
            deleteSubProcessInstanceByNodeInstanceNo(req.getProcessInstanceNo(), nodeInstanceNo, req.isRollbackSubProcess(), req.isRollbackTask());
        }
        log.info("end do rollback node " + processNodeInstance.getNodeId() + ":" + nodeInstanceNo);
    }
    
    private void deleteTaskByNodeInstanceNo(String nodeInstanceNo) {
        QueryTaskReqVO query = new QueryTaskReqVO();
        query.setNodeInstanceNo(nodeInstanceNo);
        List<ProcessTaskEntity> list = processTaskRepository.queryTask(query);
        for (ProcessTaskEntity task : list) {
            deleteOneTask(task.getTaskNo());
        }
    }
    
    private void deleteOneTask(String taskNo) {
        log.info("delete task:" + taskNo);
        processTaskRepository.deleteTaskByTaskNo(taskNo);
        List<ProcessTaskAssignEntity> assignList = processTaskRepository.findTaskAssignListByTaskNo(taskNo);
        for (ProcessTaskAssignEntity assign: assignList) {
            processTaskRepository.deleteTaskAssignByAssignNo(assign.getAssignNo());
        }
    }
    
    private void deleteSubProcessInstanceByNodeInstanceNo(String processInstanceNo, String nodeInstanceNo, boolean deleteSubProcess,
            boolean deleteTask) {
        List<ProcessInstanceEntity> processInstances = processRepository.queryProcessInstanceByParentNodeInstanceNo(processInstanceNo, nodeInstanceNo);
        for (ProcessInstanceEntity processInstance : processInstances) {
            deleteProcessInstance(processInstance.getInstanceNo(), deleteSubProcess, deleteTask);
        }
    }
    
    private void deleteProcessInstance(String processInstanceNo, boolean deleteSubProcess, boolean deleteTask) {
        AssertUtils.isNotNull(processInstanceNo);
        // delete nodes
        QueryProcessNodeReq req = new QueryProcessNodeReq();
        req.setProcessInstanceNo(processInstanceNo);
        List<ProcessNodeInstanceEntity> nodeList = processRepository.findNodeInstances(req);
        for (ProcessNodeInstanceEntity node : nodeList) {
            deleteNodeInstance(processInstanceNo, node.getNodeInstanceNo(), deleteSubProcess, deleteTask);
        }
        // delete process
        log.info("delete process instance:" + processInstanceNo);
        processRepository.deleteProcessInstanceByNo(processInstanceNo);
    }
    
    private void deleteNodeInstance(String processInstanceNo, String nodeInstanceNo, boolean deleteSubProcess,
            boolean deleteTask) {
        if (deleteTask) {
            // delete task
            deleteTaskByNodeInstanceNo(nodeInstanceNo);
        }
        if (deleteSubProcess) {
            // delete subflow
            deleteSubProcessInstanceByNodeInstanceNo(processInstanceNo, nodeInstanceNo, deleteSubProcess, deleteTask);
        }
        // delete node
        log.info("delete node instance:" + nodeInstanceNo);
        processRepository.deleteProcessNodeInstanceByNo(nodeInstanceNo);
    }

    public ProcessRepository getProcessRepository() {
        return processRepository;
    }

    public void setProcessRepository(ProcessRepository processRepository) {
        this.processRepository = processRepository;
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

    public TransactionTemplate getTransactionTemplate() {
        return transactionTemplate;
    }

    public void setTransactionTemplate(TransactionTemplate transactionTemplate) {
        this.transactionTemplate = transactionTemplate;
    }
    
    
    
}
