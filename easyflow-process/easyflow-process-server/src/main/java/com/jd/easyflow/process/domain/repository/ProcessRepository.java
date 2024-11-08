package com.jd.easyflow.process.domain.repository;

import java.util.List;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeExecutionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.vo.ProcessDefinitionForListVO;
import com.jd.easyflow.process.domain.model.vo.QueryProcessNodeReq;

/**
 * 
 * @author liyuliang5
 * 
 */
public interface ProcessRepository {

    ProcessInstanceEntity getProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo);

    ProcessInstanceEntity getActiveProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo);

    ProcessNodeInstanceEntity getOpenNodeInstance(String processInstanceNo, String nodeId);
    
    List<ProcessNodeInstanceEntity> findNodeInstances(QueryProcessNodeReq queryReq);
    
    ProcessInstanceEntity getByProcessInstanceNo(String processInstanceNo);
    
    void saveProcessInstance(ProcessInstanceEntity entity);
    
    void saveProcessInstanceWithCreatedDate(ProcessInstanceEntity entity);

    void updateProcessInstanceById(ProcessInstanceEntity entity);
    
    void updateProcessInstanceByNo(ProcessInstanceEntity entity);
    
    ProcessNodeInstanceEntity getByNodeInstanceNo(String nodeInstanceNo);
    
    void saveProcessNodeInstance(ProcessNodeInstanceEntity entity);
    
    void saveProcessNodeInstanceWithCreatedDate(ProcessNodeInstanceEntity entity);

    void updateProcessNodeInstanceById(ProcessNodeInstanceEntity entity);
    
    void updateProcessNodeInstanceByNo(ProcessNodeInstanceEntity entity);
    
    ProcessNodeExecutionEntity getByNodeExecutionNo(String nodeExecutionNo);
    
    void saveProcessNodeExecution(ProcessNodeExecutionEntity entity);
    
    void saveProcessNodeExecutionWithCreatedDate(ProcessNodeExecutionEntity entity);

    void updateProcessNodeExecutionById(ProcessNodeExecutionEntity entity);
    
    void updateProcessNodeExecutionByNo(ProcessNodeExecutionEntity entity);
    
    List<ProcessNodeInstanceEntity> findActiveNodeInstances(String processInstanceNo);
    
    List<ProcessNodeInstanceEntity> findOpenNodeInstances(String processInstanceNo);

    PagerResult<ProcessInstanceEntity> pageQueryProcessInstance(PagerCondition pagerQueryReq);

    void saveProcessDefinition(ProcessDefinitionEntity entity);

    int updateProcessDefinitionLatestById(long id);

    int updateProcessDefinitionById(ProcessDefinitionEntity processDefinition);

    PagerResult<ProcessDefinitionForListVO> pageQueryProcessDefinition(PagerCondition pagerQueryReq);

    ProcessDefinitionEntity findProcessDefinitionByDefIdAndVersion(String defId, Integer defVersion);

    ProcessDefinitionEntity findLatestProcessDefinition(String defId);

    boolean existProcessDefinition(String defId);

    List<ProcessInstanceDTO> queryProcessInstanceByInstanceNos(List<String> processInstanceNos);
    
    List<ProcessNodeInstanceEntity> queryNodeInstanceByNos(List<String> nodeInstanceNos);

}
