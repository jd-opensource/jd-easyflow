package com.jd.easyflow.process.adapter.export.converter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.jd.easyflow.process.adapter.export.dto.instance.CreateProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeExecutionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessNodeReqDTO;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeExecutionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.vo.CreateProcessInstanceReqVO;
import com.jd.easyflow.process.domain.model.vo.QueryProcessNodeReq;
import com.jd.easyflow.process.domain.model.vo.QueryProcessNodeReq.QueryProcessNodeReqBuilder;

/**
 * @author liyuliang5
 *
 */
public class ProcessInstanceConverter {

    public static final ProcessInstanceConverter INSTANCE = new ProcessInstanceConverter();
    

    public CreateProcessInstanceReqVO convert(CreateProcessInstanceReq req) {
        if ( req == null ) {
            return null;
        }

        CreateProcessInstanceReqVO createProcessInstanceReqVO = new CreateProcessInstanceReqVO();

        createProcessInstanceReqVO.setBizData( req.getBizData() );
        createProcessInstanceReqVO.setBizNo( req.getBizNo() );
        createProcessInstanceReqVO.setBizStatus( req.getBizStatus() );
        createProcessInstanceReqVO.setCreator( req.getCreator() );
        Map<String, Object> map = req.getDataMap();
        if ( map != null ) {
            createProcessInstanceReqVO.setDataMap( new HashMap<String, Object>( map ) );
        }
        createProcessInstanceReqVO.setInstanceName( req.getInstanceName() );
        createProcessInstanceReqVO.setKeyField( req.getKeyField() );
        createProcessInstanceReqVO.setKeyField2( req.getKeyField2() );
        createProcessInstanceReqVO.setParam( req.getParam() );
        createProcessInstanceReqVO.setProcessId( req.getProcessId() );
        createProcessInstanceReqVO.setProcessType( req.getProcessType() );
        createProcessInstanceReqVO.setProductCode( req.getProductCode() );

        return createProcessInstanceReqVO;
    }

    public ProcessInstanceDTO convert(ProcessInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessInstanceDTO processInstanceDTO = new ProcessInstanceDTO();

        processInstanceDTO.setBizData( entity.getBizData() );
        processInstanceDTO.setBizNo( entity.getBizNo() );
        processInstanceDTO.setBizStatus( entity.getBizStatus() );
        processInstanceDTO.setCreatedDate( entity.getCreatedDate() );
        processInstanceDTO.setCreator( entity.getCreator() );
        processInstanceDTO.setCurrentNodeIds( entity.getCurrentNodeIds() );
        processInstanceDTO.setEndTime( entity.getEndTime() );
        processInstanceDTO.setExtData( entity.getExtData() );
        processInstanceDTO.setInstanceName( entity.getInstanceName() );
        processInstanceDTO.setInstanceNo( entity.getInstanceNo() );
        processInstanceDTO.setKeyField( entity.getKeyField() );
        processInstanceDTO.setKeyField2( entity.getKeyField2() );
        processInstanceDTO.setModifiedDate( entity.getModifiedDate() );
        processInstanceDTO.setParentInstanceNo( entity.getParentInstanceNo() );
        processInstanceDTO.setParentNodeInstanceNo( entity.getParentNodeInstanceNo() );
        processInstanceDTO.setProcessDefId( entity.getProcessDefId() );
        processInstanceDTO.setProcessType( entity.getProcessType() );
        processInstanceDTO.setProductCode( entity.getProductCode() );
        processInstanceDTO.setStartTime( entity.getStartTime() );
        processInstanceDTO.setStatus( entity.getStatus() );
        processInstanceDTO.setVars( entity.getVars() );

        return processInstanceDTO;
    }

    public List<ProcessInstanceDTO> convert(List<ProcessInstanceEntity> list) {
        if ( list == null ) {
            return null;
        }

        List<ProcessInstanceDTO> list1 = new ArrayList<ProcessInstanceDTO>( list.size() );
        for ( ProcessInstanceEntity processInstanceEntity : list ) {
            list1.add( convert( processInstanceEntity ) );
        }

        return list1;
    }

    public ProcessInstanceEntity convert(ProcessInstanceDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessInstanceEntity processInstanceEntity = new ProcessInstanceEntity();

        processInstanceEntity.setBizData( dto.getBizData() );
        processInstanceEntity.setBizNo( dto.getBizNo() );
        processInstanceEntity.setBizStatus( dto.getBizStatus() );
        processInstanceEntity.setCreatedDate( dto.getCreatedDate() );
        processInstanceEntity.setCreator( dto.getCreator() );
        processInstanceEntity.setCurrentNodeIds( dto.getCurrentNodeIds() );
        processInstanceEntity.setEndTime( dto.getEndTime() );
        processInstanceEntity.setExtData( dto.getExtData() );
        processInstanceEntity.setInstanceName( dto.getInstanceName() );
        processInstanceEntity.setInstanceNo( dto.getInstanceNo() );
        processInstanceEntity.setKeyField( dto.getKeyField() );
        processInstanceEntity.setKeyField2( dto.getKeyField2() );
        processInstanceEntity.setModifiedDate( dto.getModifiedDate() );
        processInstanceEntity.setParentInstanceNo( dto.getParentInstanceNo() );
        processInstanceEntity.setParentNodeInstanceNo( dto.getParentNodeInstanceNo() );
        processInstanceEntity.setProcessDefId( dto.getProcessDefId() );
        processInstanceEntity.setProcessType( dto.getProcessType() );
        processInstanceEntity.setProductCode( dto.getProductCode() );
        processInstanceEntity.setStartTime( dto.getStartTime() );
        processInstanceEntity.setStatus( dto.getStatus() );
        processInstanceEntity.setVars( dto.getVars() );

        return processInstanceEntity;
    }

    public ProcessNodeInstanceDTO convert(ProcessNodeInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessNodeInstanceDTO processNodeInstanceDTO = new ProcessNodeInstanceDTO();

        processNodeInstanceDTO.setCreatedDate( entity.getCreatedDate() );
        processNodeInstanceDTO.setEndTime( entity.getEndTime() );
        processNodeInstanceDTO.setExecutors( entity.getExecutors() );
        processNodeInstanceDTO.setExtData( entity.getExtData() );
        processNodeInstanceDTO.setModifiedDate( entity.getModifiedDate() );
        processNodeInstanceDTO.setNextNodeInstances( entity.getNextNodeInstances() );
        processNodeInstanceDTO.setNodeId( entity.getNodeId() );
        processNodeInstanceDTO.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processNodeInstanceDTO.setPreviousNodeInstances( entity.getPreviousNodeInstances() );
        processNodeInstanceDTO.setProcessDefId( entity.getProcessDefId() );
        processNodeInstanceDTO.setProcessInstanceNo( entity.getProcessInstanceNo() );
        processNodeInstanceDTO.setProductCode( entity.getProductCode() );
        processNodeInstanceDTO.setStartTime( entity.getStartTime() );
        processNodeInstanceDTO.setStatus( entity.getStatus() );
        processNodeInstanceDTO.setVars( entity.getVars() );

        return processNodeInstanceDTO;
    }
    
    public ProcessNodeInstanceEntity convert(ProcessNodeInstanceDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessNodeInstanceEntity entity = new ProcessNodeInstanceEntity();

        entity.setCreatedDate( dto.getCreatedDate() );
        entity.setEndTime( dto.getEndTime() );
        entity.setExecutors( dto.getExecutors() );
        entity.setExtData( dto.getExtData() );
        entity.setModifiedDate( dto.getModifiedDate() );
        entity.setNextNodeInstances( dto.getNextNodeInstances() );
        entity.setNodeId( dto.getNodeId() );
        entity.setNodeInstanceNo( dto.getNodeInstanceNo() );
        entity.setPreviousNodeInstances( dto.getPreviousNodeInstances() );
        entity.setProcessDefId( dto.getProcessDefId() );
        entity.setProcessInstanceNo( dto.getProcessInstanceNo() );
        entity.setProductCode( dto.getProductCode() );
        entity.setStartTime( dto.getStartTime() );
        entity.setStatus( dto.getStatus() );
        entity.setVars( dto.getVars() );

        return entity;
    }

    public List<ProcessNodeInstanceDTO> convertNodeInstanceList(List<ProcessNodeInstanceEntity> entity) {
        if ( entity == null ) {
            return null;
        }

        List<ProcessNodeInstanceDTO> list = new ArrayList<ProcessNodeInstanceDTO>( entity.size() );
        for ( ProcessNodeInstanceEntity processNodeInstanceEntity : entity ) {
            list.add( convert( processNodeInstanceEntity ) );
        }

        return list;
    }
    
    public List<ProcessNodeExecutionDTO> convertNodeExecutionList(List<ProcessNodeExecutionEntity> entity) {
        if ( entity == null ) {
            return null;
        }

        List<ProcessNodeExecutionDTO> list = new ArrayList<ProcessNodeExecutionDTO>( entity.size() );
        for ( ProcessNodeExecutionEntity processNodeExecutionEntity : entity ) {
            list.add( convert( processNodeExecutionEntity ) );
        }

        return list;
    }

    public ProcessNodeExecutionDTO convert(ProcessNodeExecutionEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessNodeExecutionDTO processNodeExecutionDTO = new ProcessNodeExecutionDTO();

        processNodeExecutionDTO.setCreatedDate( entity.getCreatedDate() );
        processNodeExecutionDTO.setEndTime( entity.getEndTime() );
        processNodeExecutionDTO.setEventId( entity.getEventId() );
        processNodeExecutionDTO.setExecutor( entity.getExecutor() );
        processNodeExecutionDTO.setExtData( entity.getExtData() );
        processNodeExecutionDTO.setId( entity.getId() );
        processNodeExecutionDTO.setModifiedDate( entity.getModifiedDate() );
        processNodeExecutionDTO.setNextNodeInstances( entity.getNextNodeInstances() );
        processNodeExecutionDTO.setNodeExecutionNo( entity.getNodeExecutionNo() );
        processNodeExecutionDTO.setNodeId( entity.getNodeId() );
        processNodeExecutionDTO.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processNodeExecutionDTO.setProcessDefId( entity.getProcessDefId() );
        processNodeExecutionDTO.setProductCode( entity.getProductCode() );
        processNodeExecutionDTO.setStartTime( entity.getStartTime() );
        processNodeExecutionDTO.setStatus( entity.getStatus() );

        return processNodeExecutionDTO;
    }

    public QueryProcessNodeReq convert(QueryProcessNodeReqDTO reqDTO) {
        if ( reqDTO == null ) {
            return null;
        }

        QueryProcessNodeReqBuilder queryProcessNodeReq = QueryProcessNodeReq.builder();

        queryProcessNodeReq.nodeId( reqDTO.getNodeId() );
        queryProcessNodeReq.processInstanceNo( reqDTO.getProcessInstanceNo() );
        Set<String> set = reqDTO.getStatus();
        if ( set != null ) {
            queryProcessNodeReq.status( new HashSet<String>( set ) );
        }

        return queryProcessNodeReq.build();
    }

    public List<ProcessNodeInstanceDTO> convertDTOList(List<ProcessNodeInstanceEntity> entityList) {
        if ( entityList == null ) {
            return null;
        }

        List<ProcessNodeInstanceDTO> list = new ArrayList<ProcessNodeInstanceDTO>( entityList.size() );
        for ( ProcessNodeInstanceEntity processNodeInstanceEntity : entityList ) {
            list.add( convert( processNodeInstanceEntity ) );
        }

        return list;
    }
}

