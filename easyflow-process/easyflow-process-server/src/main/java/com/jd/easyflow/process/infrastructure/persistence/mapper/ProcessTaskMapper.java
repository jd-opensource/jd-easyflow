package com.jd.easyflow.process.infrastructure.persistence.mapper;

import java.util.List;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTask;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ProcessTaskMapper {
    
    int deleteByPrimaryKey(Long id);

    int insert(ProcessTask record);
    
    int insertWithCreatedDate(ProcessTask record);

    int insertSelective(ProcessTask record);

    ProcessTask selectByPrimaryKey(Long id);
    
    List<ProcessTask> list(QueryTaskReqVO query);

    int updateByPrimaryKeySelective(ProcessTask record);

    int updateByPrimaryKey(ProcessTask record);
    
    int updateByTaskNo(ProcessTask record);
    
    long countTaskAndAssignByPagerCondition(PagerCondition condition);
    
    List<ProcessTask> selectTaskAndAssignByPagerCondition(PagerCondition condition);
    
    long countTaskByPagerCondition(PagerCondition condition);
    
    List<ProcessTask> selectTaskByPagerCondition(PagerCondition condition);
}