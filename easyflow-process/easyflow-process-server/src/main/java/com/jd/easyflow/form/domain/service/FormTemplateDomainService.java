package com.jd.easyflow.form.domain.service;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.form.domain.model.entity.FormTemplateEntity;
import com.jd.easyflow.form.domain.repository.FormTemplateRepository;

/**
 * @author liyuliang5
 *
 */
public class FormTemplateDomainService {
    
    @Autowired
    FormTemplateRepository formTemplateRepository;

    public void update(FormTemplateEntity entity) {
        FormTemplateEntity formTemplate = formTemplateRepository.get(entity.getTemplateCode());
        formTemplateRepository.delete(formTemplate.getId());
        formTemplateRepository.add(entity);
    }

    public FormTemplateRepository getFormTemplateRepository() {
        return formTemplateRepository;
    }

    public void setFormTemplateRepository(FormTemplateRepository formTemplateRepository) {
        this.formTemplateRepository = formTemplateRepository;
    }
    
    
}
