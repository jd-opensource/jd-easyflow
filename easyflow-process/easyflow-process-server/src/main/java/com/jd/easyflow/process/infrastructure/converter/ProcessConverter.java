package com.jd.easyflow.process.infrastructure.converter;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeExecutionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.vo.ProcessDefinitionForListVO;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessDefinition;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessInstance;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessNodeExecution;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessNodeInstance;

/**
 * @author liyuliang5
 *
 */
public class ProcessConverter {

    public static ProcessConverter INSTANCE = new ProcessConverter();

    public ProcessInstanceEntity convert(ProcessInstance po) {
        if ( po == null ) {
            return null;
        }

        ProcessInstanceEntity processInstanceEntity = new ProcessInstanceEntity();

        processInstanceEntity.setBizData( po.getBizData() );
        processInstanceEntity.setBizNo( po.getBizNo() );
        processInstanceEntity.setBizStatus( po.getBizStatus() );
        processInstanceEntity.setCreatedDate( po.getCreatedDate() );
        processInstanceEntity.setCreator( po.getCreator() );
        processInstanceEntity.setCurrentNodeIds( po.getCurrentNodeIds() );
        processInstanceEntity.setEndTime( po.getEndTime() );
        processInstanceEntity.setExtData( po.getExtData() );
        processInstanceEntity.setId( po.getId() );
        processInstanceEntity.setInstanceName( po.getInstanceName() );
        processInstanceEntity.setInstanceNo( po.getInstanceNo() );
        processInstanceEntity.setKeyField( po.getKeyField() );
        processInstanceEntity.setKeyField2( po.getKeyField2() );
        processInstanceEntity.setModifiedDate( po.getModifiedDate() );
        processInstanceEntity.setParentInstanceNo( po.getParentInstanceNo() );
        processInstanceEntity.setParentNodeInstanceNo( po.getParentNodeInstanceNo() );
        processInstanceEntity.setProcessDefId( po.getProcessDefId() );
        processInstanceEntity.setProcessType( po.getProcessType() );
        processInstanceEntity.setProductCode( po.getProductCode() );
        processInstanceEntity.setStartTime( po.getStartTime() );
        processInstanceEntity.setStatus( po.getStatus() );
        processInstanceEntity.setVars( po.getVars() );

        return processInstanceEntity;
    }

    public ProcessInstance convert(ProcessInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessInstance processInstance = new ProcessInstance();

        processInstance.setBizData( entity.getBizData() );
        processInstance.setBizNo( entity.getBizNo() );
        processInstance.setBizStatus( entity.getBizStatus() );
        processInstance.setCreatedDate( entity.getCreatedDate() );
        processInstance.setCreator( entity.getCreator() );
        processInstance.setCurrentNodeIds( entity.getCurrentNodeIds() );
        processInstance.setEndTime( entity.getEndTime() );
        processInstance.setExtData( entity.getExtData() );
        processInstance.setId( entity.getId() );
        processInstance.setInstanceName( entity.getInstanceName() );
        processInstance.setInstanceNo( entity.getInstanceNo() );
        processInstance.setKeyField( entity.getKeyField() );
        processInstance.setKeyField2( entity.getKeyField2() );
        processInstance.setModifiedDate( entity.getModifiedDate() );
        processInstance.setParentInstanceNo( entity.getParentInstanceNo() );
        processInstance.setParentNodeInstanceNo( entity.getParentNodeInstanceNo() );
        processInstance.setProcessDefId( entity.getProcessDefId() );
        processInstance.setProcessType( entity.getProcessType() );
        processInstance.setProductCode( entity.getProductCode() );
        processInstance.setStartTime( entity.getStartTime() );
        processInstance.setStatus( entity.getStatus() );
        processInstance.setVars( entity.getVars() );

        return processInstance;
    }

    public ProcessNodeInstanceEntity convert(ProcessNodeInstance po) {
        if ( po == null ) {
            return null;
        }

        ProcessNodeInstanceEntity processNodeInstanceEntity = new ProcessNodeInstanceEntity();

        processNodeInstanceEntity.setCreatedDate( po.getCreatedDate() );
        processNodeInstanceEntity.setEndTime( po.getEndTime() );
        processNodeInstanceEntity.setExecutors( po.getExecutors() );
        processNodeInstanceEntity.setExtData( po.getExtData() );
        processNodeInstanceEntity.setId( po.getId() );
        processNodeInstanceEntity.setModifiedDate( po.getModifiedDate() );
        processNodeInstanceEntity.setNextNodeInstances( po.getNextNodeInstances() );
        processNodeInstanceEntity.setNodeId( po.getNodeId() );
        processNodeInstanceEntity.setNodeInstanceNo( po.getNodeInstanceNo() );
        processNodeInstanceEntity.setPreviousNodeInstances( po.getPreviousNodeInstances() );
        processNodeInstanceEntity.setProcessDefId( po.getProcessDefId() );
        processNodeInstanceEntity.setProcessInstanceNo( po.getProcessInstanceNo() );
        processNodeInstanceEntity.setProductCode( po.getProductCode() );
        processNodeInstanceEntity.setStartTime( po.getStartTime() );
        processNodeInstanceEntity.setStatus( po.getStatus() );
        processNodeInstanceEntity.setVars( po.getVars() );

        return processNodeInstanceEntity;
    }

    public ProcessNodeInstance convert(ProcessNodeInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessNodeInstance processNodeInstance = new ProcessNodeInstance();

        processNodeInstance.setCreatedDate( entity.getCreatedDate() );
        processNodeInstance.setEndTime( entity.getEndTime() );
        processNodeInstance.setExecutors( entity.getExecutors() );
        processNodeInstance.setExtData( entity.getExtData() );
        processNodeInstance.setId( entity.getId() );
        processNodeInstance.setModifiedDate( entity.getModifiedDate() );
        processNodeInstance.setNextNodeInstances( entity.getNextNodeInstances() );
        processNodeInstance.setNodeId( entity.getNodeId() );
        processNodeInstance.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processNodeInstance.setPreviousNodeInstances( entity.getPreviousNodeInstances() );
        processNodeInstance.setProcessDefId( entity.getProcessDefId() );
        processNodeInstance.setProcessInstanceNo( entity.getProcessInstanceNo() );
        processNodeInstance.setProductCode( entity.getProductCode() );
        processNodeInstance.setStartTime( entity.getStartTime() );
        processNodeInstance.setStatus( entity.getStatus() );
        processNodeInstance.setVars( entity.getVars() );

        return processNodeInstance;
    }

    public ProcessNodeExecutionEntity convert(ProcessNodeExecution po) {
        if ( po == null ) {
            return null;
        }

        ProcessNodeExecutionEntity processNodeExecutionEntity = new ProcessNodeExecutionEntity();

        processNodeExecutionEntity.setCreatedDate( po.getCreatedDate() );
        processNodeExecutionEntity.setEndTime( po.getEndTime() );
        processNodeExecutionEntity.setEventId( po.getEventId() );
        processNodeExecutionEntity.setExecutor( po.getExecutor() );
        processNodeExecutionEntity.setExtData( po.getExtData() );
        processNodeExecutionEntity.setId( po.getId() );
        processNodeExecutionEntity.setModifiedDate( po.getModifiedDate() );
        processNodeExecutionEntity.setNextNodeInstances( po.getNextNodeInstances() );
        processNodeExecutionEntity.setNodeExecutionNo( po.getNodeExecutionNo() );
        processNodeExecutionEntity.setNodeId( po.getNodeId() );
        processNodeExecutionEntity.setNodeInstanceNo( po.getNodeInstanceNo() );
        processNodeExecutionEntity.setProcessDefId( po.getProcessDefId() );
        processNodeExecutionEntity.setProductCode( po.getProductCode() );
        processNodeExecutionEntity.setStartTime( po.getStartTime() );
        processNodeExecutionEntity.setStatus( po.getStatus() );

        return processNodeExecutionEntity;
    }

    public ProcessNodeExecution convert(ProcessNodeExecutionEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessNodeExecution processNodeExecution = new ProcessNodeExecution();

        processNodeExecution.setCreatedDate( entity.getCreatedDate() );
        processNodeExecution.setEndTime( entity.getEndTime() );
        processNodeExecution.setEventId( entity.getEventId() );
        processNodeExecution.setExecutor( entity.getExecutor() );
        processNodeExecution.setExtData( entity.getExtData() );
        processNodeExecution.setId( entity.getId() );
        processNodeExecution.setModifiedDate( entity.getModifiedDate() );
        processNodeExecution.setNextNodeInstances( entity.getNextNodeInstances() );
        processNodeExecution.setNodeExecutionNo( entity.getNodeExecutionNo() );
        processNodeExecution.setNodeId( entity.getNodeId() );
        processNodeExecution.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processNodeExecution.setProcessDefId( entity.getProcessDefId() );
        processNodeExecution.setProductCode( entity.getProductCode() );
        processNodeExecution.setStartTime( entity.getStartTime() );
        processNodeExecution.setStatus( entity.getStatus() );

        return processNodeExecution;
    }

    public List<ProcessNodeInstanceEntity> convertToList(List<ProcessNodeInstance> po) {
        if ( po == null ) {
            return null;
        }

        List<ProcessNodeInstanceEntity> list = new ArrayList<ProcessNodeInstanceEntity>( po.size() );
        for ( ProcessNodeInstance processNodeInstance : po ) {
            list.add( convert( processNodeInstance ) );
        }

        return list;
    }

    public ProcessDefinition convert(ProcessDefinitionEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessDefinition processDefinition = new ProcessDefinition();

        processDefinition.setBizType( entity.getBizType() );
        processDefinition.setCategory( entity.getCategory() );
        processDefinition.setContent( entity.getContent() );
        processDefinition.setCreatedBy( entity.getCreatedBy() );
        processDefinition.setCreatedDate( entity.getCreatedDate() );
        processDefinition.setDefId( entity.getDefId() );
        processDefinition.setDefSource( entity.getDefSource() );
        processDefinition.setDefVersion( entity.getDefVersion() );
        processDefinition.setExtData( entity.getExtData() );
        processDefinition.setFormat( entity.getFormat() );
        processDefinition.setId( entity.getId() );
        processDefinition.setJsonContent( entity.getJsonContent() );
        processDefinition.setLatest( entity.getLatest() );
        processDefinition.setModifiedBy( entity.getModifiedBy() );
        processDefinition.setModifiedDate( entity.getModifiedDate() );
        processDefinition.setName( entity.getName() );

        return processDefinition;
    }

    public ProcessDefinitionEntity convert(ProcessDefinition processDefinition) {
        if ( processDefinition == null ) {
            return null;
        }

        ProcessDefinitionEntity processDefinitionEntity = new ProcessDefinitionEntity();

        processDefinitionEntity.setBizType( processDefinition.getBizType() );
        processDefinitionEntity.setCategory( processDefinition.getCategory() );
        processDefinitionEntity.setContent( processDefinition.getContent() );
        processDefinitionEntity.setCreatedBy( processDefinition.getCreatedBy() );
        processDefinitionEntity.setCreatedDate( processDefinition.getCreatedDate() );
        processDefinitionEntity.setDefId( processDefinition.getDefId() );
        processDefinitionEntity.setDefSource( processDefinition.getDefSource() );
        processDefinitionEntity.setDefVersion( processDefinition.getDefVersion() );
        processDefinitionEntity.setExtData( processDefinition.getExtData() );
        processDefinitionEntity.setFormat( processDefinition.getFormat() );
        processDefinitionEntity.setId( processDefinition.getId() );
        processDefinitionEntity.setJsonContent( processDefinition.getJsonContent() );
        processDefinitionEntity.setLatest( processDefinition.getLatest() );
        processDefinitionEntity.setModifiedBy( processDefinition.getModifiedBy() );
        processDefinitionEntity.setModifiedDate( processDefinition.getModifiedDate() );
        processDefinitionEntity.setName( processDefinition.getName() );

        return processDefinitionEntity;
    }

    public List<ProcessDefinitionForListVO> convertProcessDefinitionInfo(List<ProcessDefinition> processDefinitionList) {
        if ( processDefinitionList == null ) {
            return null;
        }

        List<ProcessDefinitionForListVO> list = new ArrayList<ProcessDefinitionForListVO>( processDefinitionList.size() );
        for ( ProcessDefinition processDefinition : processDefinitionList ) {
            list.add( processDefinitionToProcessDefinitionForListVO( processDefinition ) );
        }

        return list;
    }

    public List<ProcessInstanceEntity> convertProcessInstanceList(List<ProcessInstance> processInstanceList) {
        if ( processInstanceList == null ) {
            return null;
        }

        List<ProcessInstanceEntity> list = new ArrayList<ProcessInstanceEntity>( processInstanceList.size() );
        for ( ProcessInstance processInstance : processInstanceList ) {
            list.add( convert( processInstance ) );
        }

        return list;
    }

    public List<ProcessNodeInstanceEntity> convertProcessNodeInstanceList(List<ProcessNodeInstance> processNodeInstances) {
        if ( processNodeInstances == null ) {
            return null;
        }

        List<ProcessNodeInstanceEntity> list = new ArrayList<ProcessNodeInstanceEntity>( processNodeInstances.size() );
        for ( ProcessNodeInstance processNodeInstance : processNodeInstances ) {
            list.add( convert( processNodeInstance ) );
        }

        return list;
    }

    public List<ProcessNodeExecutionEntity> convertProcessNodeExecutionList(List<ProcessNodeExecution> processNodeExecutions) {
        if ( processNodeExecutions == null ) {
            return null;
        }

        List<ProcessNodeExecutionEntity> list = new ArrayList<ProcessNodeExecutionEntity>( processNodeExecutions.size() );
        for ( ProcessNodeExecution processNodeExecution : processNodeExecutions ) {
            list.add( convert( processNodeExecution ) );
        }

        return list;
    }

    public List<ProcessInstanceDTO> convertProcessInstanceDtoList(List<ProcessInstance> processInstanceList) {
        if ( processInstanceList == null ) {
            return null;
        }

        List<ProcessInstanceDTO> list = new ArrayList<ProcessInstanceDTO>( processInstanceList.size() );
        for ( ProcessInstance processInstance : processInstanceList ) {
            list.add( processInstanceToProcessInstanceDTO( processInstance ) );
        }

        return list;
    }

    public List<ProcessNodeInstanceEntity> convertNodeInstanceList(List<ProcessNodeInstance> processInstanceList) {
        if ( processInstanceList == null ) {
            return null;
        }

        List<ProcessNodeInstanceEntity> list = new ArrayList<ProcessNodeInstanceEntity>( processInstanceList.size() );
        for ( ProcessNodeInstance processNodeInstance : processInstanceList ) {
            list.add( convert( processNodeInstance ) );
        }

        return list;
    }

    protected ProcessDefinitionForListVO processDefinitionToProcessDefinitionForListVO(ProcessDefinition processDefinition) {
        if ( processDefinition == null ) {
            return null;
        }

        ProcessDefinitionForListVO processDefinitionForListVO = new ProcessDefinitionForListVO();

        processDefinitionForListVO.setBizType( processDefinition.getBizType() );
        processDefinitionForListVO.setCategory( processDefinition.getCategory() );
        processDefinitionForListVO.setCreatedBy( processDefinition.getCreatedBy() );
        processDefinitionForListVO.setCreatedDate( processDefinition.getCreatedDate() );
        processDefinitionForListVO.setDefId( processDefinition.getDefId() );
        processDefinitionForListVO.setDefSource( processDefinition.getDefSource() );
        processDefinitionForListVO.setDefVersion( processDefinition.getDefVersion() );
        processDefinitionForListVO.setFormat( processDefinition.getFormat() );
        processDefinitionForListVO.setId( processDefinition.getId() );
        processDefinitionForListVO.setLatest( processDefinition.getLatest() );
        processDefinitionForListVO.setModifiedBy( processDefinition.getModifiedBy() );
        processDefinitionForListVO.setModifiedDate( processDefinition.getModifiedDate() );
        processDefinitionForListVO.setName( processDefinition.getName() );

        return processDefinitionForListVO;
    }

    protected ProcessInstanceDTO processInstanceToProcessInstanceDTO(ProcessInstance processInstance) {
        if ( processInstance == null ) {
            return null;
        }

        ProcessInstanceDTO processInstanceDTO = new ProcessInstanceDTO();

        processInstanceDTO.setBizData( processInstance.getBizData() );
        processInstanceDTO.setBizNo( processInstance.getBizNo() );
        processInstanceDTO.setBizStatus( processInstance.getBizStatus() );
        processInstanceDTO.setCreatedDate( processInstance.getCreatedDate() );
        processInstanceDTO.setCreator( processInstance.getCreator() );
        processInstanceDTO.setCurrentNodeIds( processInstance.getCurrentNodeIds() );
        processInstanceDTO.setEndTime( processInstance.getEndTime() );
        processInstanceDTO.setExtData( processInstance.getExtData() );
        processInstanceDTO.setInstanceName( processInstance.getInstanceName() );
        processInstanceDTO.setInstanceNo( processInstance.getInstanceNo() );
        processInstanceDTO.setKeyField( processInstance.getKeyField() );
        processInstanceDTO.setKeyField2( processInstance.getKeyField2() );
        processInstanceDTO.setModifiedDate( processInstance.getModifiedDate() );
        processInstanceDTO.setParentInstanceNo( processInstance.getParentInstanceNo() );
        processInstanceDTO.setParentNodeInstanceNo( processInstance.getParentNodeInstanceNo() );
        processInstanceDTO.setProcessDefId( processInstance.getProcessDefId() );
        processInstanceDTO.setProcessType( processInstance.getProcessType() );
        processInstanceDTO.setProductCode( processInstance.getProductCode() );
        processInstanceDTO.setStartTime( processInstance.getStartTime() );
        processInstanceDTO.setStatus( processInstance.getStatus() );
        processInstanceDTO.setVars( processInstance.getVars() );

        return processInstanceDTO;
    }
}
