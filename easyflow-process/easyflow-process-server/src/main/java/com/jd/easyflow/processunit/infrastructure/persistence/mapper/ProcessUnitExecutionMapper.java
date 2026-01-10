package com.jd.easyflow.processunit.infrastructure.persistence.mapper;

import java.util.Collection;
import java.util.List;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitExecution;

/**
 * @author liyuliang5
 * 
 */
@Mapper
public interface ProcessUnitExecutionMapper {

    int insert(ProcessUnitExecution record);

    ProcessUnitExecution selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(ProcessUnitExecution record);

    int updateByPrimaryKey(ProcessUnitExecution record);

    int updateByExecutionNo(ProcessUnitExecution record);

    ProcessUnitExecution getByExecutionNo(String executionNo);

    List<ProcessUnitExecution> selectProcessUnitExecution(@Param("instanceNos") Collection<String> instanceNos);


    int physicsDeleteProcessUnitExecution(@Param("ids") Collection<Long> ids);

    List<ProcessUnitExecution> selectProcessUnitExecutionByPageCondition(PagerCondition pagerQueryReq);

    long countProcessUnitExecutionByPageCondition(PagerCondition pagerQueryReq);
    
    int insertForMigration(ProcessUnitExecution record);
    
    int updateForMigration(ProcessUnitExecution record);


}