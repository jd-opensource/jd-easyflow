package com.jd.easyflow.process.infrastructure.repository;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeExecutionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.vo.ProcessDefinitionForListVO;
import com.jd.easyflow.process.domain.model.vo.QueryProcessNodeReq;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.process.infrastructure.converter.ProcessConverter;
import com.jd.easyflow.process.infrastructure.persistence.mapper.ProcessDefinitionMapper;
import com.jd.easyflow.process.infrastructure.persistence.mapper.ProcessInstanceMapper;
import com.jd.easyflow.process.infrastructure.persistence.mapper.ProcessNodeExecutionMapper;
import com.jd.easyflow.process.infrastructure.persistence.mapper.ProcessNodeInstanceMapper;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessDefinition;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessInstance;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessNodeExecution;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessNodeInstance;

/**
 * @author liyuliang5
 *
 */
public class ProcessRepositoryImpl implements ProcessRepository {

    @Autowired
    private ProcessInstanceMapper processInstanceMapper;

    @Autowired
    private ProcessNodeInstanceMapper processNodeInstanceMapper;

    @Autowired
    private ProcessNodeExecutionMapper processNodeExecutionMapper;

    @Autowired
    private ProcessDefinitionMapper processDefinitionMapper;
    
    private static final Integer MAX_QUERY_INSTANCENOS_NUM = 1000;

    @Override
    public ProcessInstanceEntity getProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo) {
        ProcessInstance processInstance = processInstanceMapper.getProcessInstanceByProcessTypeAndBizNo(processType,
                bizNo);
        return ProcessConverter.INSTANCE.convert(processInstance);
    }
    
    @Override
    public ProcessInstanceEntity getActiveProcessInstanceByProcessTypeAndBizNo(String processType, String bizNo) {
        ProcessInstance processInstance = processInstanceMapper.getActiveProcessInstanceByProcessTypeAndBizNo(processType,
                bizNo);
        return ProcessConverter.INSTANCE.convert(processInstance);
    }

    @Override
    public ProcessNodeInstanceEntity getOpenNodeInstance(String processInstanceNo, String nodeId) {
        ProcessNodeInstance instance = processNodeInstanceMapper.getOpenNodeInstance(processInstanceNo, nodeId);
        return ProcessConverter.INSTANCE.convert(instance);
    }
    
    @Override
    public List<ProcessNodeInstanceEntity> findNodeInstances(QueryProcessNodeReq queryReq) {
        List<ProcessNodeInstance> instanceList = processNodeInstanceMapper.findNodeInstances(queryReq);
        return ProcessConverter.INSTANCE.convertToList(instanceList);
    }
    
    @Override
    public void saveProcessInstance(ProcessInstanceEntity entity) {
        ProcessInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processInstanceMapper.insert(instance);
        entity.setId(instance.getId());
    }

    @Override
    public void updateProcessInstanceById(ProcessInstanceEntity entity) {
        ProcessInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processInstanceMapper.updateByPrimaryKey(instance);
    }
    
    @Override
    public void updateProcessInstanceByNo(ProcessInstanceEntity entity) {
        ProcessInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processInstanceMapper.updateByProcessInstanceNo(instance);
    }

    @Override
    public void saveProcessNodeInstance(ProcessNodeInstanceEntity entity) {
        ProcessNodeInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processNodeInstanceMapper.insert(instance);
        entity.setId(instance.getId());
    }
    
    @Override
    public void updateProcessNodeInstanceById(ProcessNodeInstanceEntity entity) {
        ProcessNodeInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processNodeInstanceMapper.updateByPrimaryKey(instance);
    }
    
    @Override
    public void updateProcessNodeInstanceByNo(ProcessNodeInstanceEntity entity) {
        ProcessNodeInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processNodeInstanceMapper.updateByProcessNodeInstanceNo(instance);
    }
    
    @Override
    public void saveProcessNodeExecution(ProcessNodeExecutionEntity entity) {
        ProcessNodeExecution execution = ProcessConverter.INSTANCE.convert(entity);
        processNodeExecutionMapper.insert(execution);
        entity.setId(execution.getId());
    }

    @Override
    public void updateProcessNodeExecutionById(ProcessNodeExecutionEntity entity) {
        ProcessNodeExecution execution = ProcessConverter.INSTANCE.convert(entity);
        processNodeExecutionMapper.updateByPrimaryKey(execution);

    }
    
    public void updateProcessNodeExecutionByNo(ProcessNodeExecutionEntity entity) {
        ProcessNodeExecution execution = ProcessConverter.INSTANCE.convert(entity);
        processNodeExecutionMapper.updateByProcessNodeExecutionNo(execution);

    }

    @Override
    public ProcessInstanceEntity getByProcessInstanceNo(String processInstanceNo) {
        ProcessInstance instance = processInstanceMapper.selectByInstanceNo(processInstanceNo);
         return ProcessConverter.INSTANCE.convert(instance);
    }

    @Override
    public ProcessNodeInstanceEntity getByNodeInstanceNo(String nodeInstanceNo) {
        ProcessNodeInstance instance = processNodeInstanceMapper.selectByNodeInstanceNo(nodeInstanceNo);
        return ProcessConverter.INSTANCE.convert(instance);
    }

    @Override
    public ProcessNodeExecutionEntity getByNodeExecutionNo(String nodeExecutionNo) {
        ProcessNodeExecution execution = processNodeExecutionMapper.selectByNodeExecutionNo(nodeExecutionNo);
        return ProcessConverter.INSTANCE.convert(execution);
    }

    @Override
    public List<ProcessNodeInstanceEntity> findActiveNodeInstances(String processInstanceNo) {
        List<ProcessNodeInstance> instances = processNodeInstanceMapper.findActiveNodeInstances(processInstanceNo);
        return ProcessConverter.INSTANCE.convertToList(instances);
    }

    @Override
    public List<ProcessNodeInstanceEntity> findOpenNodeInstances(String processInstanceNo) {
        List<ProcessNodeInstance> instances = processNodeInstanceMapper.findOpenNodeInstances(processInstanceNo);
        return ProcessConverter.INSTANCE.convertToList(instances);
    }

    @Override
    public PagerResult<ProcessInstanceEntity> pageQueryProcessInstance(PagerCondition pagerQueryReq) {
        long count = processInstanceMapper.countProcessInstanceByPagerCondition(pagerQueryReq);
        List<ProcessInstance> processInstances = processInstanceMapper.selectProcessInstanceByPageCondition(pagerQueryReq);
        PagerResult<ProcessInstanceEntity> result = new PagerResult<>();
        result.setCount(count);
        result.setPageSize(pagerQueryReq.getPageSize());
        result.setPageNum((int) pagerQueryReq.getPageIndex());
        List<ProcessInstanceEntity> entityList = ProcessConverter.INSTANCE.convertProcessInstanceList(processInstances);
        result.setList(entityList);
        return result;
    }

    @Override
    public void saveProcessDefinition(ProcessDefinitionEntity entity) {
        ProcessDefinition processDefinition = ProcessConverter.INSTANCE.convert(entity);
        processDefinitionMapper.insert(processDefinition);
    }

    @Override
    public int updateProcessDefinitionLatestById(long id) {
        return processDefinitionMapper.updateProcessDefinitionLatestStatus(id);
    }

    @Override
    public int updateProcessDefinitionById(ProcessDefinitionEntity entity) {
        ProcessDefinition processDefinition = ProcessConverter.INSTANCE.convert(entity);
        return processDefinitionMapper.updateByPrimaryKeySelective(processDefinition);
    }

    @Override
    public PagerResult<ProcessDefinitionForListVO> pageQueryProcessDefinition(PagerCondition pagerQueryReq) {
        long count = processDefinitionMapper.countProcessDefByPageCondition(pagerQueryReq);
        List<ProcessDefinition> processDefinitionList = processDefinitionMapper.selectProcessDefByPageCondition(pagerQueryReq);
        PagerResult<ProcessDefinitionForListVO> result = new PagerResult<>();
        result.setCount(count);
        result.setPageSize(pagerQueryReq.getPageSize());
        result.setPageNum((int) pagerQueryReq.getPageIndex());
        List<ProcessDefinitionForListVO> entityList = ProcessConverter.INSTANCE.convertProcessDefinitionInfo(processDefinitionList);
        result.setList(entityList);
        return result;
    }

    @Override
    public ProcessDefinitionEntity findProcessDefinitionByDefIdAndVersion(String defId, Integer defVersion) {
        AssertUtils.isNotBlank(defId);
        ProcessDefinition processDefinition = processDefinitionMapper.selectByDefIdAndVersion(defId, defVersion);
        return ProcessConverter.INSTANCE.convert(processDefinition);
    }

    @Override
    public ProcessDefinitionEntity findLatestProcessDefinition(String defId) {
        ProcessDefinition latestDefinition = processDefinitionMapper.findLatestDefinition(defId);
        return ProcessConverter.INSTANCE.convert(latestDefinition);
    }

    @Override
    public boolean existProcessDefinition(String defId) {
        AssertUtils.isNotBlank(defId);
        return processDefinitionMapper.existProcessDefinition(defId);
    }

    @Override
    public void saveProcessInstanceWithCreatedDate(ProcessInstanceEntity entity) {
        ProcessInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processInstanceMapper.insertWithCreatedDate(instance);
        entity.setId(instance.getId());
    }

    @Override
    public void saveProcessNodeInstanceWithCreatedDate(ProcessNodeInstanceEntity entity) {
        ProcessNodeInstance instance = ProcessConverter.INSTANCE.convert(entity);
        processNodeInstanceMapper.insertWithCreatedDate(instance);
        entity.setId(instance.getId());
    }

    @Override
    public void saveProcessNodeExecutionWithCreatedDate(ProcessNodeExecutionEntity entity) {
        ProcessNodeExecution execution = ProcessConverter.INSTANCE.convert(entity);
        processNodeExecutionMapper.insertWithCreatedDate(execution);
        entity.setId(execution.getId());
    }

    @Override
    public List<ProcessInstanceDTO> queryProcessInstanceByInstanceNos(List<String> processInstanceNos) {
        if (0 == processInstanceNos.size() || processInstanceNos.size() > MAX_QUERY_INSTANCENOS_NUM) {
            throw new IllegalArgumentException("queryProcessInstanceByInstanceNos range is 1-" + MAX_QUERY_INSTANCENOS_NUM + "");
        }
        List<ProcessInstance> processInstances = processInstanceMapper.queryProcessInstanceByInstanceNos(processInstanceNos);
        return ProcessConverter.INSTANCE.convertProcessInstanceDtoList(processInstances);
    }
    
    @Override
    public List<ProcessNodeInstanceEntity> queryNodeInstanceByNos(List<String> nodeInstanceNos) {
        List<ProcessNodeInstance> processNodeInstances = processNodeInstanceMapper.queryNodeInstanceByNos(nodeInstanceNos);
        return ProcessConverter.INSTANCE.convertNodeInstanceList(processNodeInstances);
    }

    @Override
    public List<ProcessInstanceDTO> queryProcessInstanceByParentInstanceNo(String parentInstanceNo) {
        List<ProcessInstance> processInstances = processInstanceMapper.selectProcessInstanceByParentInstanceNo(parentInstanceNo);
        return ProcessConverter.INSTANCE.convertProcessInstanceDtoList(processInstances);
    }

}
