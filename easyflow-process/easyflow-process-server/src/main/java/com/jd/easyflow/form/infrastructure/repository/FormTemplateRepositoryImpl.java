package com.jd.easyflow.form.infrastructure.repository;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.form.domain.model.entity.FormTemplateEntity;
import com.jd.easyflow.form.domain.repository.FormTemplateRepository;
import com.jd.easyflow.form.infrastructure.converter.FormTemplateConverter;
import com.jd.easyflow.form.infrastructure.persistence.mapper.FormTemplateMapper;
import com.jd.easyflow.form.infrastructure.persistence.po.FormTemplate;

/**
 * @author liyuliang5
 *
 */
public class FormTemplateRepositoryImpl implements FormTemplateRepository {
    
    @Autowired
    private FormTemplateMapper formTemplateMapper;

    @Override
    public PagerResult find(PagerCondition pagerCondition) {
        long count = formTemplateMapper.countByPagerCondition(pagerCondition);
        List<FormTemplate> list = formTemplateMapper.selectByPagerCondition(pagerCondition);
        return new PagerResult<>(count, FormTemplateConverter.INSTANCE.convert(list));
    }

    @Override
    public void add(FormTemplateEntity formTemplate) {
        FormTemplate po = FormTemplateConverter.INSTANCE.convert(formTemplate);
        formTemplateMapper.insert(po);
        
    }

    @Override
    public void update(FormTemplateEntity formTemplate) {
        FormTemplate po = FormTemplateConverter.INSTANCE.convert(formTemplate);
        formTemplateMapper.updateByPrimaryKey(po);
    }

    @Override
    public void delete(long id) {
        formTemplateMapper.deleteByPrimaryKey(id);
        
    }

    @Override
    public FormTemplateEntity get(String templateCode) {
        FormTemplate po = formTemplateMapper.selectByTemplateCode(templateCode);
        return FormTemplateConverter.INSTANCE.convert(po);
    }

}
