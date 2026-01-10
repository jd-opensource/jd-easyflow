 package com.jd.easyflow.processunit.infrastructure.converter;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitExecution;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitInstance;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitConverter {
    
    public static ProcessUnitConverter INSTANCE = new ProcessUnitConverter();
    
    public ProcessUnitInstanceEntity po2Entity(ProcessUnitInstance instance) {
        if ( instance == null ) {
            return null;
        }

        ProcessUnitInstanceEntity processUnitInstanceEntity = new ProcessUnitInstanceEntity();

        processUnitInstanceEntity.setAutoRunFlag( instance.getAutoRunFlag() );
        processUnitInstanceEntity.setAutoRunTimes( instance.getAutoRunTimes() );
        processUnitInstanceEntity.setBizNo( instance.getBizNo() );
        processUnitInstanceEntity.setCreatedDate( instance.getCreatedDate() );
        processUnitInstanceEntity.setVars(instance.getVars());
        processUnitInstanceEntity.setExtData( instance.getExtData() );
        processUnitInstanceEntity.setId( instance.getId() );
        processUnitInstanceEntity.setInstanceNo( instance.getInstanceNo() );
        processUnitInstanceEntity.setModifiedDate( instance.getModifiedDate() );
        processUnitInstanceEntity.setNextAutoRunTime( instance.getNextAutoRunTime() );
        processUnitInstanceEntity.setParentNo( instance.getParentNo() );
        processUnitInstanceEntity.setProcessUnitCode( instance.getProcessUnitCode() );
        processUnitInstanceEntity.setProductCode( instance.getProductCode() );
        processUnitInstanceEntity.setRequestContent( instance.getRequestContent() );
        processUnitInstanceEntity.setResponseContent( instance.getResponseContent() );
        processUnitInstanceEntity.setResult( instance.getResult() );
        processUnitInstanceEntity.setId(instance.getId());
        return processUnitInstanceEntity;
    }

    public ProcessUnitInstance entity2Po(ProcessUnitInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessUnitInstance processUnitInstance = new ProcessUnitInstance();

        processUnitInstance.setAutoRunFlag( entity.getAutoRunFlag() );
        processUnitInstance.setAutoRunTimes( entity.getAutoRunTimes() );
        processUnitInstance.setBizNo( entity.getBizNo() );
        processUnitInstance.setCreatedDate( entity.getCreatedDate() );
        processUnitInstance.setVars(entity.getVars());
        processUnitInstance.setExtData( entity.getExtData() );
        processUnitInstance.setId( entity.getId() );
        processUnitInstance.setInstanceNo( entity.getInstanceNo() );
        processUnitInstance.setModifiedDate( entity.getModifiedDate() );
        processUnitInstance.setNextAutoRunTime( entity.getNextAutoRunTime() );
        processUnitInstance.setParentNo( entity.getParentNo() );
        processUnitInstance.setProcessUnitCode( entity.getProcessUnitCode() );
        processUnitInstance.setProductCode( entity.getProductCode() );
        processUnitInstance.setRequestContent( entity.getRequestContent() );
        processUnitInstance.setResponseContent( entity.getResponseContent() );
        processUnitInstance.setResult( entity.getResult() );

        return processUnitInstance;
    }

    public ProcessUnitExecutionEntity po2Entity(ProcessUnitExecution execution) {
        if ( execution == null ) {
            return null;
        }

        ProcessUnitExecutionEntity processUnitExecutionEntity = new ProcessUnitExecutionEntity();

        processUnitExecutionEntity.setCreatedDate( execution.getCreatedDate() );
        if ( execution.getElaspeTime() != null ) {
            processUnitExecutionEntity.setElaspeTime( execution.getElaspeTime() );
        }
        processUnitExecutionEntity.setExecType( execution.getExecType() );
        processUnitExecutionEntity.setExecutionNo( execution.getExecutionNo() );
        processUnitExecutionEntity.setExtData( execution.getExtData() );
        processUnitExecutionEntity.setId( execution.getId() );
        processUnitExecutionEntity.setInstanceNo( execution.getInstanceNo() );
        processUnitExecutionEntity.setModifiedDate( execution.getModifiedDate() );
        processUnitExecutionEntity.setParentNo( execution.getParentNo() );
        processUnitExecutionEntity.setProcessUnitCode( execution.getProcessUnitCode() );
        processUnitExecutionEntity.setBizNo(execution.getBizNo());
        processUnitExecutionEntity.setProductCode( execution.getProductCode() );
        processUnitExecutionEntity.setRequestContent( execution.getRequestContent() );
        processUnitExecutionEntity.setRequestNo( execution.getRequestNo() );
        processUnitExecutionEntity.setRequestTime( execution.getRequestTime() );
        processUnitExecutionEntity.setResponseContent( execution.getResponseContent() );
        processUnitExecutionEntity.setResponseTime( execution.getResponseTime() );
        processUnitExecutionEntity.setResult( execution.getResult() );

        return processUnitExecutionEntity;
    }

    public ProcessUnitExecution entity2Po(ProcessUnitExecutionEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessUnitExecution processUnitExecution = new ProcessUnitExecution();

        processUnitExecution.setCreatedDate( entity.getCreatedDate() );
        processUnitExecution.setElaspeTime( entity.getElaspeTime() );
        processUnitExecution.setExecType( entity.getExecType() );
        processUnitExecution.setExecutionNo( entity.getExecutionNo() );
        processUnitExecution.setExtData( entity.getExtData() );
        processUnitExecution.setId( entity.getId() );
        processUnitExecution.setInstanceNo( entity.getInstanceNo() );
        processUnitExecution.setModifiedDate( entity.getModifiedDate() );
        processUnitExecution.setParentNo( entity.getParentNo() );
        processUnitExecution.setProcessUnitCode( entity.getProcessUnitCode() );
        processUnitExecution.setBizNo(entity.getBizNo());
        processUnitExecution.setProductCode( entity.getProductCode() );
        processUnitExecution.setRequestContent( entity.getRequestContent() );
        processUnitExecution.setRequestNo( entity.getRequestNo() );
        processUnitExecution.setRequestTime( entity.getRequestTime() );
        processUnitExecution.setResponseContent( entity.getResponseContent() );
        processUnitExecution.setResponseTime( entity.getResponseTime() );
        processUnitExecution.setResult( entity.getResult() );

        return processUnitExecution;
    }
    
    public List<ProcessUnitExecutionEntity> executionPoList2EntityList(List<ProcessUnitExecution> poList) {
        if (poList == null) {
            return null;
        }
        List<ProcessUnitExecutionEntity> entityList = new ArrayList<ProcessUnitExecutionEntity>();
        poList.forEach(po -> entityList.add(po2Entity(po)));
        return entityList;
    }
    
    public List<ProcessUnitInstanceEntity> instancePoList2EntityList(List<ProcessUnitInstance> poList) {
        if (poList == null) {
            return null;
        }
        List<ProcessUnitInstanceEntity> entityList = new ArrayList<ProcessUnitInstanceEntity>();
        poList.forEach(po -> entityList.add(po2Entity(po)));
        return entityList;
    }
    
}

