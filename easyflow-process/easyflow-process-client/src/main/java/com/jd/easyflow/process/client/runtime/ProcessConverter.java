package com.jd.easyflow.process.client.runtime;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;

/**
 * @author liyuliang5
 *
 */
public class ProcessConverter {
    
    public static ProcessConverter INSTANCE = new ProcessConverter();

    public void copy(ProcessInstanceDTO source, ProcessInstanceDTO target) {
        if ( source == null ) {
            return;
        }

        target.setBizData( source.getBizData() );
        target.setBizNo( source.getBizNo() );
        target.setBizStatus( source.getBizStatus() );
        target.setCreatedDate( source.getCreatedDate() );
        target.setCreator( source.getCreator() );
        target.setCurrentNodeIds( source.getCurrentNodeIds() );
        target.setEndTime( source.getEndTime() );
        target.setExtData( source.getExtData() );
        target.setInstanceName( source.getInstanceName() );
        target.setInstanceNo( source.getInstanceNo() );
        target.setKeyField( source.getKeyField() );
        target.setKeyField2( source.getKeyField2() );
        target.setModifiedDate( source.getModifiedDate() );
        target.setParentInstanceNo( source.getParentInstanceNo() );
        target.setParentNodeInstanceNo( source.getParentNodeInstanceNo() );
        target.setProcessDefId( source.getProcessDefId() );
        target.setProcessType( source.getProcessType() );
        target.setProductCode( source.getProductCode() );
        target.setStartTime( source.getStartTime() );
        target.setStatus( source.getStatus() );
        target.setVars( source.getVars() );
    }}
