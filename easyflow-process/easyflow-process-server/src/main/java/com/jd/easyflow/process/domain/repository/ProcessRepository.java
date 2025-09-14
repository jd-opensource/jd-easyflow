package com.jd.easyflow.process.domain.repository;

import java.util.List;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
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
    
    /*ProcessInstance*/
    
    void saveProcessInstance(ProcessInstanceEntity entity);
    
    void saveProcessInstanceWithCreatedDate(ProcessInstanceEntity entity);
    
    void updateProcessInstanceById(ProcessInstanceEntity entity);
    
    void updateProcessInstanceByNo(ProcessInstanceEntity entity);
    
    void deleteProcessInstanceByNo(String processInstanceNo);

    ProcessInstanceEntity getProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo);

    ProcessInstanceEntity getActiveProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo);
    
    ProcessInstanceEntity getByProcessInstanceNo(String processInstanceNo);
    
    PagerResult<ProcessInstanceEntity> pageQueryProcessInstance(PagerCondition pagerQueryReq);
    
    List<ProcessInstanceEntity> queryProcessInstanceByInstanceNos(List<String> processInstanceNos);
    
    List<ProcessInstanceEntity> queryProcessInstanceByParentInstanceNo(String parentProcessInstanceNo);
    List<ProcessInstanceEntity> queryProcessInstanceByParentNodeInstanceNo(String parentInstanceNo, String parentNodeInstanceNo);

    /*NodeInstance*/
    
    void saveProcessNodeInstance(ProcessNodeInstanceEntity entity);
    
    void saveProcessNodeInstanceWithCreatedDate(ProcessNodeInstanceEntity entity);

    void updateProcessNodeInstanceById(ProcessNodeInstanceEntity entity);
    
    void updateProcessNodeInstanceByNo(ProcessNodeInstanceEntity entity);
    
    void deleteProcessNodeInstanceByNo(String processNodeInstanceNo);
    
    ProcessNodeInstanceEntity getOpenNodeInstance(String processInstanceNo, String nodeId);
    
    List<ProcessNodeInstanceEntity> findNodeInstances(QueryProcessNodeReq queryReq);
    
    ProcessNodeInstanceEntity getByNodeInstanceNo(String nodeInstanceNo);
    
   List<ProcessNodeInstanceEntity> findActiveNodeInstances(String processInstanceNo);
    
    List<ProcessNodeInstanceEntity> findOpenNodeInstances(String processInstanceNo);
    
    List<ProcessNodeInstanceEntity> queryNodeInstanceByNos(List<String> nodeInstanceNos);
    
    /*NodeExecution*/
    
    ProcessNodeExecutionEntity getByNodeExecutionNo(String nodeExecutionNo);
    
    void saveProcessNodeExecution(ProcessNodeExecutionEntity entity);
    
    void saveProcessNodeExecutionWithCreatedDate(ProcessNodeExecutionEntity entity);

    void updateProcessNodeExecutionById(ProcessNodeExecutionEntity entity);
    
    void updateProcessNodeExecutionByNo(ProcessNodeExecutionEntity entity);
    
 
    /*ProcessDefinition*/

    void saveProcessDefinition(ProcessDefinitionEntity entity);

    int updateProcessDefinitionLatestById(long id);

    int updateProcessDefinitionById(ProcessDefinitionEntity processDefinition);

    PagerResult<ProcessDefinitionForListVO> pageQueryProcessDefinition(PagerCondition pagerQueryReq);

    ProcessDefinitionEntity findProcessDefinitionByDefIdAndVersion(String defId, Integer defVersion);

    ProcessDefinitionEntity findLatestProcessDefinition(String defId);

    boolean existProcessDefinition(String defId);
    
    

}
