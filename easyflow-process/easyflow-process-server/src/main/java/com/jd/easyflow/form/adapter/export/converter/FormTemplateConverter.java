package com.jd.easyflow.form.adapter.export.converter;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.form.adapter.export.dto.FormTemplateDTO;
import com.jd.easyflow.form.domain.model.entity.FormTemplateEntity;

/**
 * @author liyuliang5
 *
 */
public class FormTemplateConverter {
    
    public static FormTemplateConverter INSTANCE = new FormTemplateConverter();

    public FormTemplateDTO convert(FormTemplateEntity entity) {
        if ( entity == null ) {
            return null;
        }

        FormTemplateDTO formTemplateDTO = new FormTemplateDTO();

        formTemplateDTO.setBizType( entity.getBizType() );
        formTemplateDTO.setConfig( entity.getConfig() );
        formTemplateDTO.setCreatedDate( entity.getCreatedDate() );
        formTemplateDTO.setId( entity.getId() );
        formTemplateDTO.setModifiedDate( entity.getModifiedDate() );
        formTemplateDTO.setStatus( entity.getStatus() );
        formTemplateDTO.setTemplateCode( entity.getTemplateCode() );
        formTemplateDTO.setTemplateName( entity.getTemplateName() );

        return formTemplateDTO;
    }

    public FormTemplateEntity convert(FormTemplateDTO dto) {
        if ( dto == null ) {
            return null;
        }

        FormTemplateEntity formTemplateEntity = new FormTemplateEntity();

        formTemplateEntity.setBizType( dto.getBizType() );
        formTemplateEntity.setConfig( dto.getConfig() );
        formTemplateEntity.setCreatedDate( dto.getCreatedDate() );
        formTemplateEntity.setId( dto.getId() );
        formTemplateEntity.setModifiedDate( dto.getModifiedDate() );
        formTemplateEntity.setStatus( dto.getStatus() );
        formTemplateEntity.setTemplateCode( dto.getTemplateCode() );
        formTemplateEntity.setTemplateName( dto.getTemplateName() );

        return formTemplateEntity;
    }

    public List<FormTemplateDTO> convert(List<FormTemplateEntity> list) {
        if ( list == null ) {
            return null;
        }

        List<FormTemplateDTO> list1 = new ArrayList<FormTemplateDTO>( list.size() );
        for ( FormTemplateEntity formTemplateEntity : list ) {
            list1.add( convert( formTemplateEntity ) );
        }

        return list1;
    }
}