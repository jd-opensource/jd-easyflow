package com.jd.easyflow.admin.process.adapter.page.converter;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.admin.process.adapter.page.dto.ProcessTaskInfoForPagerDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskConverter {
    
    public static ProcessTaskConverter INSTANCE = new ProcessTaskConverter();


    public List<ProcessTaskInfoForPagerDTO> convertPagerResult(List<ProcessTaskDTO> list) {
        if ( list == null ) {
            return null;
        }

        List<ProcessTaskInfoForPagerDTO> list1 = new ArrayList<ProcessTaskInfoForPagerDTO>( list.size() );
        for ( ProcessTaskDTO processTaskDTO : list ) {
            list1.add( processTaskDTOToProcessTaskInfoForPagerDTO( processTaskDTO ) );
        }

        return list1;
    }

    protected ProcessTaskInfoForPagerDTO processTaskDTOToProcessTaskInfoForPagerDTO(ProcessTaskDTO processTaskDTO) {
        if ( processTaskDTO == null ) {
            return null;
        }

        ProcessTaskInfoForPagerDTO processTaskInfoForPagerDTO = new ProcessTaskInfoForPagerDTO();

        processTaskInfoForPagerDTO.setAssignInfo( processTaskDTO.getAssignInfo() );
        List<ProcessTaskAssignDTO> list = processTaskDTO.getAssignList();
        if ( list != null ) {
            processTaskInfoForPagerDTO.setAssignList( new ArrayList<ProcessTaskAssignDTO>( list ) );
        }
        processTaskInfoForPagerDTO.setAssignTime( processTaskDTO.getAssignTime() );
        processTaskInfoForPagerDTO.setAssignType( processTaskDTO.getAssignType() );
        processTaskInfoForPagerDTO.setBizNo( processTaskDTO.getBizNo() );
        processTaskInfoForPagerDTO.setCreatedDate( processTaskDTO.getCreatedDate() );
        processTaskInfoForPagerDTO.setExecuteBizData( processTaskDTO.getExecuteBizData() );
        processTaskInfoForPagerDTO.setExecuteBizResult( processTaskDTO.getExecuteBizResult() );
        processTaskInfoForPagerDTO.setExecuteTime( processTaskDTO.getExecuteTime() );
        processTaskInfoForPagerDTO.setExecutor( processTaskDTO.getExecutor() );
        processTaskInfoForPagerDTO.setExtData( processTaskDTO.getExtData() );
        processTaskInfoForPagerDTO.setModifiedDate( processTaskDTO.getModifiedDate() );
        processTaskInfoForPagerDTO.setNodeExecutionNo( processTaskDTO.getNodeExecutionNo() );
        processTaskInfoForPagerDTO.setNodeInstanceNo( processTaskDTO.getNodeInstanceNo() );
        processTaskInfoForPagerDTO.setProcessInstanceNo( processTaskDTO.getProcessInstanceNo() );
        processTaskInfoForPagerDTO.setProcessType( processTaskDTO.getProcessType() );
        processTaskInfoForPagerDTO.setProductCode( processTaskDTO.getProductCode() );
        processTaskInfoForPagerDTO.setStatus( processTaskDTO.getStatus() );
        processTaskInfoForPagerDTO.setTaskBizCode( processTaskDTO.getTaskBizCode() );
        processTaskInfoForPagerDTO.setTaskBizName( processTaskDTO.getTaskBizName() );
        processTaskInfoForPagerDTO.setTaskNo( processTaskDTO.getTaskNo() );
        processTaskInfoForPagerDTO.setTaskType( processTaskDTO.getTaskType() );

        return processTaskInfoForPagerDTO;
    }
}

