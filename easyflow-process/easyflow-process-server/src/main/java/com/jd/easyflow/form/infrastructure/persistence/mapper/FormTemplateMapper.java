package com.jd.easyflow.form.infrastructure.persistence.mapper;

import java.util.List;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.form.infrastructure.persistence.po.FormTemplate;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FormTemplateMapper {
    
    FormTemplate selectByTemplateCode(String templateCode);
    
    int deleteByPrimaryKey(Long id);

    int insert(FormTemplate record);

    int insertSelective(FormTemplate record);

    FormTemplate selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(FormTemplate record);

    int updateByPrimaryKey(FormTemplate record);
    
    long countByPagerCondition(PagerCondition condition);

    List<FormTemplate> selectByPagerCondition(PagerCondition condition);
}