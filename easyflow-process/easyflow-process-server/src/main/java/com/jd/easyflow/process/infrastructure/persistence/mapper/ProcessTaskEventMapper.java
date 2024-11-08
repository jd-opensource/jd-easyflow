package com.jd.easyflow.process.infrastructure.persistence.mapper;

import java.util.List;

import com.jd.easyflow.process.domain.model.vo.QueryTaskEventReqVO;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTaskEvent;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ProcessTaskEventMapper {
    
    int deleteByPrimaryKey(Long id);

    int insert(ProcessTaskEvent record);
    
    int insertWithCreatedDate(ProcessTaskEvent record);

    int insertSelective(ProcessTaskEvent record);

    ProcessTaskEvent selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(ProcessTaskEvent record);

    int updateByPrimaryKey(ProcessTaskEvent record);
    
    int updateByTaskEventNo(ProcessTaskEvent record);
    
    List<ProcessTaskEvent> list(QueryTaskEventReqVO query);
    
}