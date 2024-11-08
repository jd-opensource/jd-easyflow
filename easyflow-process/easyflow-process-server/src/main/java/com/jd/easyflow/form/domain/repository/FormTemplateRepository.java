package com.jd.easyflow.form.domain.repository;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.form.domain.model.entity.FormTemplateEntity;

/**
 * @author liyuliang5
 *
 */
public interface FormTemplateRepository {

    PagerResult find(PagerCondition pagerCondition);
    
    void add(FormTemplateEntity formTemplate);
    
    void update(FormTemplateEntity formTemplate);
    
    void delete(long id);
    
    FormTemplateEntity get(String templateCode);
}
