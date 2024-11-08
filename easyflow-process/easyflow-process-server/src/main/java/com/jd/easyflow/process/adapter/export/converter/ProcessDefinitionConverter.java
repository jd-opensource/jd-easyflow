package com.jd.easyflow.process.adapter.export.converter;

import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.vo.ProcessDefinitionForListVO;
import java.util.ArrayList;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class ProcessDefinitionConverter {
    
    public static ProcessDefinitionConverter INSTANCE = new ProcessDefinitionConverter();

    public ProcessDefinitionDTO convert(ProcessDefinitionEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessDefinitionDTO processDefinitionDTO = new ProcessDefinitionDTO();

        processDefinitionDTO.setBizType( entity.getBizType() );
        processDefinitionDTO.setCategory( entity.getCategory() );
        processDefinitionDTO.setContent( entity.getContent() );
        processDefinitionDTO.setCreatedBy( entity.getCreatedBy() );
        processDefinitionDTO.setCreatedDate( entity.getCreatedDate() );
        processDefinitionDTO.setDefId( entity.getDefId() );
        processDefinitionDTO.setDefSource( entity.getDefSource() );
        processDefinitionDTO.setDefVersion( entity.getDefVersion() );
        processDefinitionDTO.setExtData( entity.getExtData() );
        processDefinitionDTO.setFormat( entity.getFormat() );
        processDefinitionDTO.setId( entity.getId() );
        processDefinitionDTO.setJsonContent( entity.getJsonContent() );
        processDefinitionDTO.setLatest( entity.getLatest() );
        processDefinitionDTO.setModifiedBy( entity.getModifiedBy() );
        processDefinitionDTO.setModifiedDate( entity.getModifiedDate() );
        processDefinitionDTO.setName( entity.getName() );

        return processDefinitionDTO;
    }

    public ProcessDefinitionEntity convert(ProcessDefinitionDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessDefinitionEntity processDefinitionEntity = new ProcessDefinitionEntity();

        processDefinitionEntity.setBizType( dto.getBizType() );
        processDefinitionEntity.setCategory( dto.getCategory() );
        processDefinitionEntity.setContent( dto.getContent() );
        processDefinitionEntity.setCreatedBy( dto.getCreatedBy() );
        processDefinitionEntity.setCreatedDate( dto.getCreatedDate() );
        processDefinitionEntity.setDefId( dto.getDefId() );
        processDefinitionEntity.setDefSource( dto.getDefSource() );
        processDefinitionEntity.setDefVersion( dto.getDefVersion() );
        processDefinitionEntity.setExtData( dto.getExtData() );
        processDefinitionEntity.setFormat( dto.getFormat() );
        processDefinitionEntity.setId( dto.getId() );
        processDefinitionEntity.setJsonContent( dto.getJsonContent() );
        processDefinitionEntity.setLatest( dto.getLatest() );
        processDefinitionEntity.setModifiedBy( dto.getModifiedBy() );
        processDefinitionEntity.setModifiedDate( dto.getModifiedDate() );
        processDefinitionEntity.setName( dto.getName() );

        return processDefinitionEntity;
    }

    public PagerResult<ProcessDefinitionDTO> convert(com.jd.easyflow.common.dto.pager.PagerResult<ProcessDefinitionForListVO> pagerResult) {
        if ( pagerResult == null ) {
            return null;
        }

        PagerResult<ProcessDefinitionDTO> pagerResult1 = new PagerResult<ProcessDefinitionDTO>();

        pagerResult1.setCount( pagerResult.getCount() );
        pagerResult1.setPageNum( pagerResult.getPageNum() );
        pagerResult1.setPageSize( pagerResult.getPageSize() );

        pagerResult1.setList( convertList(pagerResult.getList()) );

        return pagerResult1;
    }

    public List<ProcessDefinitionDTO> convertList(List<ProcessDefinitionForListVO> processDefinitionListDTOS) {
        if ( processDefinitionListDTOS == null ) {
            return null;
        }

        List<ProcessDefinitionDTO> list = new ArrayList<ProcessDefinitionDTO>( processDefinitionListDTOS.size() );
        for ( ProcessDefinitionForListVO processDefinitionForListVO : processDefinitionListDTOS ) {
            list.add( processDefinitionForListVOToProcessDefinitionDTO( processDefinitionForListVO ) );
        }

        return list;
    }

    protected ProcessDefinitionDTO processDefinitionForListVOToProcessDefinitionDTO(ProcessDefinitionForListVO processDefinitionForListVO) {
        if ( processDefinitionForListVO == null ) {
            return null;
        }

        ProcessDefinitionDTO processDefinitionDTO = new ProcessDefinitionDTO();

        processDefinitionDTO.setBizType( processDefinitionForListVO.getBizType() );
        processDefinitionDTO.setCategory( processDefinitionForListVO.getCategory() );
        processDefinitionDTO.setCreatedBy( processDefinitionForListVO.getCreatedBy() );
        processDefinitionDTO.setCreatedDate( processDefinitionForListVO.getCreatedDate() );
        processDefinitionDTO.setDefId( processDefinitionForListVO.getDefId() );
        processDefinitionDTO.setDefSource( processDefinitionForListVO.getDefSource() );
        processDefinitionDTO.setDefVersion( processDefinitionForListVO.getDefVersion() );
        processDefinitionDTO.setFormat( processDefinitionForListVO.getFormat() );
        processDefinitionDTO.setId( processDefinitionForListVO.getId() );
        processDefinitionDTO.setLatest( processDefinitionForListVO.getLatest() );
        processDefinitionDTO.setModifiedBy( processDefinitionForListVO.getModifiedBy() );
        processDefinitionDTO.setModifiedDate( processDefinitionForListVO.getModifiedDate() );
        processDefinitionDTO.setName( processDefinitionForListVO.getName() );

        return processDefinitionDTO;
    }
}
