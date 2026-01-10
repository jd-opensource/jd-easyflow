package com.jd.easyflow.processunit.infrastructure.repository;

import java.util.List;

import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;

/**
 * 
 * @author liyuliang5
 */
public interface ProcessUnitConfigCache {

    public ProcessUnitEntity getProcessUnit(String key);
    
    public List<ProcessUnitEntity> getAllProcessUnitList();
    
}
