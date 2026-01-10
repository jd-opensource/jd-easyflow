package com.jd.easyflow.processunit.adapter.export.converter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.jd.easyflow.processunit.adapter.export.dto.ExecPolicyDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecutionDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ShardingInfoDTO;
import com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallReq;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.domain.model.vo.ExecPolicy;
import com.jd.easyflow.processunit.domain.model.vo.SyncAfterCallRes;
import com.jd.easyflow.processunit.domain.model.vo.SyncBeforeCallReq;
import com.jd.easyflow.processunit.domain.model.vo.SyncBeforeCallRes;
import com.jd.easyflow.sharding.service.ShardingComputeResult;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitConverter {

    public static ProcessUnitConverter INSTANCE = new ProcessUnitConverter();
    

    public SyncBeforeCallReq convert(com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallReq req) {
        if ( req == null ) {
            return null;
        }

        SyncBeforeCallReq syncBeforeCallReq = new SyncBeforeCallReq();

        syncBeforeCallReq.setBizNo( req.getBizNo() );
        syncBeforeCallReq.setProductCode( req.getProductCode() );
        syncBeforeCallReq.setRequestContent( req.getRequestContent() );
        syncBeforeCallReq.setRequestNo( req.getRequestNo() );
        syncBeforeCallReq.setUnitCode( req.getUnitCode() );
        syncBeforeCallReq.setVersion( req.getVersion() );
        syncBeforeCallReq.setClientInfo(req.getClientInfo());
        return syncBeforeCallReq;
    }

    public com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallRes convert(SyncBeforeCallRes res) {
        if ( res == null ) {
            return null;
        }

        com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallRes syncBeforeCallRes = new com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallRes();

        Map<String, String> map = res.getContextData();
        if ( map != null ) {
            syncBeforeCallRes.setContextData( new HashMap<String, String>( map ) );
        }
        syncBeforeCallRes.setExecPolicy( execPolicyToExecPolicyDTO( res.getExecPolicy() ) );
        syncBeforeCallRes.setExecutionNo( res.getExecutionNo() );
        syncBeforeCallRes.setInstanceNo( res.getInstanceNo() );
        Map<String, Object> map1 = res.getUnitConf();
        if ( map1 != null ) {
            syncBeforeCallRes.setUnitConf( new HashMap<String, Object>( map1 ) );
        }
        Map<String, String> map2 = res.getVariables();
        if ( map2 != null ) {
            syncBeforeCallRes.setVariables( new HashMap<String, String>( map2 ) );
        }

        return syncBeforeCallRes;
    }

    public com.jd.easyflow.processunit.domain.model.vo.SyncAfterCallReq convert(SyncAfterCallReq req) {
        if ( req == null ) {
            return null;
        }

        com.jd.easyflow.processunit.domain.model.vo.SyncAfterCallReq syncAfterCallReq = new com.jd.easyflow.processunit.domain.model.vo.SyncAfterCallReq();

        syncAfterCallReq.setBizNo( req.getBizNo() );
        Map<String, String> map = req.getContextData();
        if ( map != null ) {
            syncAfterCallReq.setContextData( new HashMap<String, String>( map ) );
        }
        syncAfterCallReq.setExecutionNo( req.getExecutionNo() );
        syncAfterCallReq.setInstanceNo( req.getInstanceNo() );
        syncAfterCallReq.setResponseContent( req.getResponseContent() );
        syncAfterCallReq.setResult( req.getResult() );
        syncAfterCallReq.setUnitCode( req.getUnitCode() );
        Map<String, String> map1 = req.getVariables();
        if ( map1 != null ) {
            syncAfterCallReq.setVariables( new HashMap<String, String>( map1 ) );
        }
        syncAfterCallReq.setAutoRunFlag(req.getAutoRunFlag());
        syncAfterCallReq.setNextAutoRunTime(req.getNextAutoRunTime());
        return syncAfterCallReq;
    }

    public com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallRes convert(SyncAfterCallRes res) {
        if ( res == null ) {
            return null;
        }

        com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallRes syncAfterCallRes = new com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallRes();

        return syncAfterCallRes;
    }

    public ProcessUnitInstanceDTO convert(ProcessUnitInstanceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessUnitInstanceDTO processUnitInstanceDTO = new ProcessUnitInstanceDTO();

        processUnitInstanceDTO.setAutoRunFlag( entity.getAutoRunFlag() );
        processUnitInstanceDTO.setAutoRunTimes( entity.getAutoRunTimes() );
        processUnitInstanceDTO.setBizNo( entity.getBizNo() );
        processUnitInstanceDTO.setCreatedDate( entity.getCreatedDate() );
        processUnitInstanceDTO.setVars(entity.getVars());
        processUnitInstanceDTO.setExtData( entity.getExtData() );
        processUnitInstanceDTO.setInstanceNo( entity.getInstanceNo() );
        processUnitInstanceDTO.setModifiedDate( entity.getModifiedDate() );
        processUnitInstanceDTO.setNextAutoRunTime( entity.getNextAutoRunTime() );
        processUnitInstanceDTO.setParentNo( entity.getParentNo() );
        processUnitInstanceDTO.setProcessUnitCode( entity.getProcessUnitCode() );
        processUnitInstanceDTO.setProductCode( entity.getProductCode() );
        processUnitInstanceDTO.setRequestContent( entity.getRequestContent() );
        processUnitInstanceDTO.setResponseContent( entity.getResponseContent() );
        processUnitInstanceDTO.setResult( entity.getResult() );
        processUnitInstanceDTO.setId(entity.getId());
        return processUnitInstanceDTO;
    }

    public ProcessUnitInstanceEntity convert(ProcessUnitInstanceDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessUnitInstanceEntity processUnitInstanceEntity = new ProcessUnitInstanceEntity();

        processUnitInstanceEntity.setAutoRunFlag( dto.getAutoRunFlag() );
        processUnitInstanceEntity.setAutoRunTimes( dto.getAutoRunTimes() );
        processUnitInstanceEntity.setBizNo( dto.getBizNo() );
        processUnitInstanceEntity.setCreatedDate( dto.getCreatedDate() );
        processUnitInstanceEntity.setVars(dto.getVars());
        processUnitInstanceEntity.setExtData( dto.getExtData() );
        processUnitInstanceEntity.setInstanceNo( dto.getInstanceNo() );
        processUnitInstanceEntity.setModifiedDate( dto.getModifiedDate() );
        processUnitInstanceEntity.setNextAutoRunTime( dto.getNextAutoRunTime() );
        processUnitInstanceEntity.setParentNo( dto.getParentNo() );
        processUnitInstanceEntity.setProcessUnitCode( dto.getProcessUnitCode() );
        processUnitInstanceEntity.setProductCode( dto.getProductCode() );
        processUnitInstanceEntity.setRequestContent( dto.getRequestContent() );
        processUnitInstanceEntity.setResponseContent( dto.getResponseContent() );
        processUnitInstanceEntity.setResult( dto.getResult() );

        return processUnitInstanceEntity;
    }

    protected ExecPolicyDTO execPolicyToExecPolicyDTO(ExecPolicy execPolicy) {
        if ( execPolicy == null ) {
            return null;
        }

        ExecPolicyDTO execPolicyDTO1 = new ExecPolicyDTO();

        execPolicyDTO1.setPolicyData( execPolicy.getPolicyData() );
        execPolicyDTO1.setPolicyType( execPolicy.getPolicyType() );
        execPolicyDTO1.setRequestContent( execPolicy.getRequestContent() );
        execPolicyDTO1.setResponseContent( execPolicy.getResponseContent() );
        execPolicyDTO1.setResult( execPolicy.getResult() );

        return execPolicyDTO1;
    }
    
    public List<ProcessUnitInstanceDTO> convertInstanceList(List<ProcessUnitInstanceEntity> entityList) {
        if (entityList == null) {
            return null;
        }
        List<ProcessUnitInstanceDTO> list = new ArrayList<ProcessUnitInstanceDTO>();
        entityList.forEach(entity -> list.add(convert(entity)));
        return list;
    }
    
    public List<ProcessUnitExecutionDTO> convertExecutionList(List<ProcessUnitExecutionEntity> entityList) {
        if (entityList == null) {
            return null;
        }
        List<ProcessUnitExecutionDTO> list = new ArrayList<ProcessUnitExecutionDTO>();
        entityList.forEach(entity -> list.add(convert(entity)));
        return list;
    }
    
    public ProcessUnitExecutionDTO convert(ProcessUnitExecutionEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessUnitExecutionDTO processUnitExecutionDTO = new ProcessUnitExecutionDTO();

        processUnitExecutionDTO.setExecutionNo( entity.getExecutionNo() );
        processUnitExecutionDTO.setRequestNo( entity.getRequestNo() );
        processUnitExecutionDTO.setParentNo( entity.getParentNo() );
        processUnitExecutionDTO.setCreatedDate( entity.getCreatedDate() );
        processUnitExecutionDTO.setExtData( entity.getExtData() );
        processUnitExecutionDTO.setInstanceNo( entity.getInstanceNo() );
        processUnitExecutionDTO.setModifiedDate( entity.getModifiedDate() );
        processUnitExecutionDTO.setResult( entity.getResult() );
        processUnitExecutionDTO.setParentNo( entity.getParentNo() );
        processUnitExecutionDTO.setProcessUnitCode( entity.getProcessUnitCode() );
        processUnitExecutionDTO.setBizNo( entity.getBizNo() );
        processUnitExecutionDTO.setProductCode( entity.getProductCode() );
        processUnitExecutionDTO.setRequestContent( entity.getRequestContent() );
        processUnitExecutionDTO.setResponseContent( entity.getResponseContent() );
        processUnitExecutionDTO.setExecType( entity.getExecType() );
        processUnitExecutionDTO.setRequestTime( entity.getRequestTime() );
        processUnitExecutionDTO.setResponseTime( entity.getResponseTime() );
        processUnitExecutionDTO.setElaspeTime( entity.getElaspeTime() );

        return processUnitExecutionDTO;
    }
    
    public ProcessUnitDTO convert(ProcessUnitEntity entity) {
        if (entity == null) {
            return null;
        }
        ProcessUnitDTO processUnitDTO = new ProcessUnitDTO();
        processUnitDTO.setName(entity.getName());
        processUnitDTO.setParentCode(entity.getParentCode());
        processUnitDTO.setProcessUnitCode(entity.getProcessUnitCode());
        processUnitDTO.setStatus(entity.getStatus());
        return processUnitDTO;
    }
    
    public List<ProcessUnitDTO> convertProcessUnitList(List<ProcessUnitEntity> entityList) {
        if (entityList == null) {
            return null;
        }
        List<ProcessUnitDTO> dtoList = new ArrayList<ProcessUnitDTO>();
        for (ProcessUnitEntity entity : entityList) {
            dtoList.add(convert(entity));
        }
        return dtoList;
    }
    
    public ShardingInfoDTO convert(ShardingComputeResult info) {
        if (info == null) {
            return null;
        }
        ShardingInfoDTO dto = new ShardingInfoDTO();
        dto.setTableSuffix(info.getTableSuffix());
        dto.setDb(info.getDb());
        dto.setShard(info.getShard());
        
        dto.setSlaveDb(info.getSlaveDb());
        dto.setSlaveTableSuffix(info.getSlaveTableSuffix());
        dto.setSlaveShard(info.getSlaveShard());
        return dto;
    }
    
}

