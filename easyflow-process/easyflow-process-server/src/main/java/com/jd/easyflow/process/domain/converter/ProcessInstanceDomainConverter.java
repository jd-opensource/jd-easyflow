package com.jd.easyflow.process.domain.converter;

import com.jd.easyflow.process.adapter.export.dto.instance.CanCancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeExecutionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.message.ProcessInstanceStatusMessage;
import com.jd.easyflow.process.adapter.message.ProcessNodeInstanceStatusMessage;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeExecutionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;

/**
 * @author liyuliang5
 *
 */
public class ProcessInstanceDomainConverter {

    public static ProcessInstanceDomainConverter INSTANCE = new ProcessInstanceDomainConverter();


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

    public ProcessNodeInstanceEntity convert(ProcessNodeInstanceDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessNodeInstanceEntity processNodeInstanceEntity = new ProcessNodeInstanceEntity();

        processNodeInstanceEntity.setCreatedDate( dto.getCreatedDate() );
        processNodeInstanceEntity.setEndTime( dto.getEndTime() );
        processNodeInstanceEntity.setExecutors( dto.getExecutors() );
        processNodeInstanceEntity.setExtData( dto.getExtData() );
        processNodeInstanceEntity.setModifiedDate( dto.getModifiedDate() );
        processNodeInstanceEntity.setNextNodeInstances( dto.getNextNodeInstances() );
        processNodeInstanceEntity.setNodeId( dto.getNodeId() );
        processNodeInstanceEntity.setNodeInstanceNo( dto.getNodeInstanceNo() );
        processNodeInstanceEntity.setPreviousNodeInstances( dto.getPreviousNodeInstances() );
        processNodeInstanceEntity.setProcessDefId( dto.getProcessDefId() );
        processNodeInstanceEntity.setProcessInstanceNo( dto.getProcessInstanceNo() );
        processNodeInstanceEntity.setProductCode( dto.getProductCode() );
        processNodeInstanceEntity.setStartTime( dto.getStartTime() );
        processNodeInstanceEntity.setStatus( dto.getStatus() );
        processNodeInstanceEntity.setVars( dto.getVars() );

        return processNodeInstanceEntity;
    }

    public ProcessNodeExecutionEntity convert(ProcessNodeExecutionDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessNodeExecutionEntity processNodeExecutionEntity = new ProcessNodeExecutionEntity();

        processNodeExecutionEntity.setCreatedDate( dto.getCreatedDate() );
        processNodeExecutionEntity.setEndTime( dto.getEndTime() );
        processNodeExecutionEntity.setEventId( dto.getEventId() );
        processNodeExecutionEntity.setExecutor( dto.getExecutor() );
        processNodeExecutionEntity.setExtData( dto.getExtData() );
        processNodeExecutionEntity.setId( dto.getId() );
        processNodeExecutionEntity.setModifiedDate( dto.getModifiedDate() );
        processNodeExecutionEntity.setNextNodeInstances( dto.getNextNodeInstances() );
        processNodeExecutionEntity.setNodeExecutionNo( dto.getNodeExecutionNo() );
        processNodeExecutionEntity.setNodeId( dto.getNodeId() );
        processNodeExecutionEntity.setNodeInstanceNo( dto.getNodeInstanceNo() );
        processNodeExecutionEntity.setProcessDefId( dto.getProcessDefId() );
        processNodeExecutionEntity.setProductCode( dto.getProductCode() );
        processNodeExecutionEntity.setStartTime( dto.getStartTime() );
        processNodeExecutionEntity.setStatus( dto.getStatus() );

        return processNodeExecutionEntity;
    }

    public ProcessInstanceStatusMessage convert2Msg(ProcessInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessInstanceStatusMessage processInstanceStatusMessage = new ProcessInstanceStatusMessage();

        processInstanceStatusMessage.setBizData( entity.getBizData() );
        processInstanceStatusMessage.setBizNo( entity.getBizNo() );
        processInstanceStatusMessage.setBizStatus( entity.getBizStatus() );
        processInstanceStatusMessage.setCreatedDate( entity.getCreatedDate() );
        processInstanceStatusMessage.setCreator( entity.getCreator() );
        processInstanceStatusMessage.setCurrentNodeIds( entity.getCurrentNodeIds() );
        processInstanceStatusMessage.setEndTime( entity.getEndTime() );
        processInstanceStatusMessage.setExtData( entity.getExtData() );
        processInstanceStatusMessage.setInstanceName( entity.getInstanceName() );
        processInstanceStatusMessage.setInstanceNo( entity.getInstanceNo() );
        processInstanceStatusMessage.setModifiedDate( entity.getModifiedDate() );
        processInstanceStatusMessage.setParentInstanceNo( entity.getParentInstanceNo() );
        processInstanceStatusMessage.setParentNodeInstanceNo( entity.getParentNodeInstanceNo() );
        processInstanceStatusMessage.setProcessDefId( entity.getProcessDefId() );
        processInstanceStatusMessage.setProcessType( entity.getProcessType() );
        processInstanceStatusMessage.setProductCode( entity.getProductCode() );
        processInstanceStatusMessage.setStartTime( entity.getStartTime() );
        processInstanceStatusMessage.setStatus( entity.getStatus() );
        processInstanceStatusMessage.setVars( entity.getVars() );

        return processInstanceStatusMessage;
    }

    public ProcessNodeInstanceStatusMessage convert2Msg(ProcessNodeInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessNodeInstanceStatusMessage processNodeInstanceStatusMessage = new ProcessNodeInstanceStatusMessage();

        processNodeInstanceStatusMessage.setCreatedDate( entity.getCreatedDate() );
        processNodeInstanceStatusMessage.setEndTime( entity.getEndTime() );
        processNodeInstanceStatusMessage.setExecutors( entity.getExecutors() );
        processNodeInstanceStatusMessage.setExtData( entity.getExtData() );
        processNodeInstanceStatusMessage.setModifiedDate( entity.getModifiedDate() );
        processNodeInstanceStatusMessage.setNextNodeInstances( entity.getNextNodeInstances() );
        processNodeInstanceStatusMessage.setNodeId( entity.getNodeId() );
        processNodeInstanceStatusMessage.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processNodeInstanceStatusMessage.setPreviousNodeInstances( entity.getPreviousNodeInstances() );
        processNodeInstanceStatusMessage.setProcessDefId( entity.getProcessDefId() );
        processNodeInstanceStatusMessage.setProcessInstanceNo( entity.getProcessInstanceNo() );
        processNodeInstanceStatusMessage.setProductCode( entity.getProductCode() );
        processNodeInstanceStatusMessage.setStartTime( entity.getStartTime() );
        processNodeInstanceStatusMessage.setStatus( entity.getStatus() );
        processNodeInstanceStatusMessage.setVars( entity.getVars() );

        return processNodeInstanceStatusMessage;
    }

    public CanCancelProcessInstanceReq convert(CancelProcessInstanceReq req) {
        if ( req == null ) {
            return null;
        }

        CanCancelProcessInstanceReq canCancelProcessInstanceReq = new CanCancelProcessInstanceReq();

        canCancelProcessInstanceReq.setCancelUser( req.getCancelUser() );
        canCancelProcessInstanceReq.setInstanceNo( req.getInstanceNo() );

        return canCancelProcessInstanceReq;
    }
}
