package com.jd.easyflow.process.infrastructure.persistence.mapper;

import com.jd.easyflow.process.infrastructure.persistence.po.ProcessNodeExecution;

/**
 * @author liyuliang5
 *
 */
public interface ProcessNodeExecutionMapper {
    int deleteByPrimaryKey(Long id);

    int insert(ProcessNodeExecution record);
    
    int insertWithCreatedDate(ProcessNodeExecution record);

    int insertSelective(ProcessNodeExecution record);

    ProcessNodeExecution selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(ProcessNodeExecution record);

    int updateByPrimaryKey(ProcessNodeExecution record);
    
    int updateByProcessNodeExecutionNo(ProcessNodeExecution record);
    
    ProcessNodeExecution selectByNodeExecutionNo(String nodeExecutionNo);

}