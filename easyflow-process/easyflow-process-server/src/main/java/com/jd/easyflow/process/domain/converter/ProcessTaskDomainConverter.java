package com.jd.easyflow.process.domain.converter;

import com.jd.easyflow.process.adapter.message.ProcessTaskStatusMessage;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTaskDomainConverter {
    
    public static ProcessTaskDomainConverter INSTANCE = new ProcessTaskDomainConverter();
    

    public ProcessTaskStatusMessage convert2Msg(ProcessTaskEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessTaskStatusMessage processTaskStatusMessage = new ProcessTaskStatusMessage();

        processTaskStatusMessage.setAssignInfo( entity.getAssignInfo() );
        processTaskStatusMessage.setAssignTime( entity.getAssignTime() );
        processTaskStatusMessage.setAssignType( entity.getAssignType() );
        processTaskStatusMessage.setBizNo( entity.getBizNo() );
        processTaskStatusMessage.setCreatedDate( entity.getCreatedDate() );
        processTaskStatusMessage.setCreator( entity.getCreator() );
        processTaskStatusMessage.setExecuteBizData( entity.getExecuteBizData() );
        processTaskStatusMessage.setExecuteBizResult( entity.getExecuteBizResult() );
        processTaskStatusMessage.setExecuteTime( entity.getExecuteTime() );
        processTaskStatusMessage.setExecutor( entity.getExecutor() );
        processTaskStatusMessage.setExtData( entity.getExtData() );
        processTaskStatusMessage.setModifiedDate( entity.getModifiedDate() );
        processTaskStatusMessage.setNodeExecutionNo( entity.getNodeExecutionNo() );
        processTaskStatusMessage.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processTaskStatusMessage.setProcessInstanceNo( entity.getProcessInstanceNo() );
        processTaskStatusMessage.setProcessType( entity.getProcessType() );
        processTaskStatusMessage.setProductCode( entity.getProductCode() );
        processTaskStatusMessage.setStatus( entity.getStatus() );
        processTaskStatusMessage.setTaskBizCode( entity.getTaskBizCode() );
        processTaskStatusMessage.setTaskBizName( entity.getTaskBizName() );
        processTaskStatusMessage.setTaskNo( entity.getTaskNo() );
        processTaskStatusMessage.setTaskType( entity.getTaskType() );

        return processTaskStatusMessage;
    }
}
