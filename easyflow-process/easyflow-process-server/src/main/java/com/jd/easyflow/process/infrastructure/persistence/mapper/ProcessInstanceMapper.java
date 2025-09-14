package com.jd.easyflow.process.infrastructure.persistence.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessInstance;

/**
 * @author liyuliang5
 *
 */
public interface ProcessInstanceMapper {
    
    int deleteByPrimaryKey(Long id);

    int insert(ProcessInstance record);
    
    int insertWithCreatedDate(ProcessInstance record);

    int insertSelective(ProcessInstance record);

    ProcessInstance selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(ProcessInstance record);

    int updateByPrimaryKey(ProcessInstance record);
    
    int updateByProcessInstanceNo(ProcessInstance record);
    
    int deleteByProcessInstanceNo(String processInstanceNo);
    
    ProcessInstance getProcessInstanceByProcessTypeAndBizNo(@Param("processType") String processType, @Param("bizNo") String bizNo);
    
    ProcessInstance getActiveProcessInstanceByProcessTypeAndBizNo(@Param("processType") String processType, @Param("bizNo") String bizNo);
    
    ProcessInstance selectByInstanceNo(String instanceNo);

    long countProcessInstanceByPagerCondition(PagerCondition pagerQueryReq);

    List<ProcessInstance> selectProcessInstanceByPageCondition(PagerCondition pagerQueryReq);
    List<ProcessInstance> queryProcessInstanceByInstanceNos(@Param("processInstanceNos") List<String> processInstanceNos);
    List<ProcessInstance> selectProcessInstanceByParentInstanceNo(String parentInstanceNo);
    List<ProcessInstance> selectProcessInstanceByParentNodeInstanceNo(@Param("parentInstanceNo") String parentInstanceNo, @Param("parentNodeInstanceNo") String parentNodeInstanceNo);

}