package com.jd.easyflow.form.infrastructure.converter;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.form.domain.model.entity.FormTemplateEntity;
import com.jd.easyflow.form.infrastructure.persistence.po.FormTemplate;

/**
 * @author liyuliang5
 *
 */
public class FormTemplateConverter {
    
    public static FormTemplateConverter INSTANCE = new FormTemplateConverter();


    public FormTemplateEntity convert(FormTemplate formTemplate) {
        if ( formTemplate == null ) {
            return null;
        }

        FormTemplateEntity formTemplateEntity = new FormTemplateEntity();

        formTemplateEntity.setBizType( formTemplate.getBizType() );
        formTemplateEntity.setConfig( formTemplate.getConfig() );
        formTemplateEntity.setCreatedDate( formTemplate.getCreatedDate() );
        formTemplateEntity.setId( formTemplate.getId() );
        formTemplateEntity.setModifiedDate( formTemplate.getModifiedDate() );
        formTemplateEntity.setStatus( formTemplate.getStatus() );
        formTemplateEntity.setTemplateCode( formTemplate.getTemplateCode() );
        formTemplateEntity.setTemplateName( formTemplate.getTemplateName() );

        return formTemplateEntity;
    }

    public FormTemplate convert(FormTemplateEntity entity) {
        if ( entity == null ) {
            return null;
        }

        FormTemplate formTemplate = new FormTemplate();

        formTemplate.setBizType( entity.getBizType() );
        formTemplate.setConfig( entity.getConfig() );
        formTemplate.setCreatedDate( entity.getCreatedDate() );
        formTemplate.setId( entity.getId() );
        formTemplate.setModifiedDate( entity.getModifiedDate() );
        formTemplate.setStatus( entity.getStatus() );
        formTemplate.setTemplateCode( entity.getTemplateCode() );
        formTemplate.setTemplateName( entity.getTemplateName() );

        return formTemplate;
    }

    public List<FormTemplateEntity> convert(List<FormTemplate> list) {
        if ( list == null ) {
            return null;
        }

        List<FormTemplateEntity> list1 = new ArrayList<FormTemplateEntity>( list.size() );
        for ( FormTemplate formTemplate : list ) {
            list1.add( convert( formTemplate ) );
        }

        return list1;
    }
}
