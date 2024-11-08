package com.jd.easyflow.admin.process.adapter.page.converter;

import com.jd.easyflow.admin.process.adapter.page.dto.ProcessInstanceInfoForPagerDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import java.util.ArrayList;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class ProcessInstanceConverter {
    
    public static ProcessInstanceConverter INSTANCE = new ProcessInstanceConverter();

    public List<ProcessInstanceInfoForPagerDTO> convertPagerResult(List<ProcessInstanceDTO> list) {
        if ( list == null ) {
            return null;
        }

        List<ProcessInstanceInfoForPagerDTO> list1 = new ArrayList<ProcessInstanceInfoForPagerDTO>( list.size() );
        for ( ProcessInstanceDTO processInstanceDTO : list ) {
            list1.add( processInstanceDTOToProcessInstanceInfoForPagerDTO( processInstanceDTO ) );
        }

        return list1;
    }

    protected ProcessInstanceInfoForPagerDTO processInstanceDTOToProcessInstanceInfoForPagerDTO(ProcessInstanceDTO processInstanceDTO) {
        if ( processInstanceDTO == null ) {
            return null;
        }

        ProcessInstanceInfoForPagerDTO processInstanceInfoForPagerDTO = new ProcessInstanceInfoForPagerDTO();

        processInstanceInfoForPagerDTO.setBizData( processInstanceDTO.getBizData() );
        processInstanceInfoForPagerDTO.setBizNo( processInstanceDTO.getBizNo() );
        processInstanceInfoForPagerDTO.setBizStatus( processInstanceDTO.getBizStatus() );
        processInstanceInfoForPagerDTO.setCreatedDate( processInstanceDTO.getCreatedDate() );
        processInstanceInfoForPagerDTO.setCreator( processInstanceDTO.getCreator() );
        processInstanceInfoForPagerDTO.setCurrentNodeIds( processInstanceDTO.getCurrentNodeIds() );
        processInstanceInfoForPagerDTO.setEndTime( processInstanceDTO.getEndTime() );
        processInstanceInfoForPagerDTO.setExtData( processInstanceDTO.getExtData() );
        processInstanceInfoForPagerDTO.setInstanceName( processInstanceDTO.getInstanceName() );
        processInstanceInfoForPagerDTO.setInstanceNo( processInstanceDTO.getInstanceNo() );
        processInstanceInfoForPagerDTO.setKeyField( processInstanceDTO.getKeyField() );
        processInstanceInfoForPagerDTO.setKeyField2( processInstanceDTO.getKeyField2() );
        processInstanceInfoForPagerDTO.setModifiedDate( processInstanceDTO.getModifiedDate() );
        processInstanceInfoForPagerDTO.setParentInstanceNo( processInstanceDTO.getParentInstanceNo() );
        processInstanceInfoForPagerDTO.setParentNodeInstanceNo( processInstanceDTO.getParentNodeInstanceNo() );
        processInstanceInfoForPagerDTO.setProcessDefId( processInstanceDTO.getProcessDefId() );
        processInstanceInfoForPagerDTO.setProcessType( processInstanceDTO.getProcessType() );
        processInstanceInfoForPagerDTO.setProductCode( processInstanceDTO.getProductCode() );
        processInstanceInfoForPagerDTO.setStartTime( processInstanceDTO.getStartTime() );
        processInstanceInfoForPagerDTO.setStatus( processInstanceDTO.getStatus() );
        processInstanceInfoForPagerDTO.setVars( processInstanceDTO.getVars() );

        return processInstanceInfoForPagerDTO;
    }
}
