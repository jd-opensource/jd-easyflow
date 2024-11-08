package com.jd.easyflow.process.infrastructure.persistence.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.process.domain.model.vo.QueryProcessNodeReq;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessNodeInstance;

/**
 * @author liyuliang5
 *
 */
public interface ProcessNodeInstanceMapper {
    int deleteByPrimaryKey(Long id);

    int insert(ProcessNodeInstance record);
    
    int insertWithCreatedDate(ProcessNodeInstance record);

    int insertSelective(ProcessNodeInstance record);

    ProcessNodeInstance selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(ProcessNodeInstance record);

    int updateByPrimaryKey(ProcessNodeInstance record);
    
    int updateByProcessNodeInstanceNo(ProcessNodeInstance record);
    
    ProcessNodeInstance getOpenNodeInstance(@Param("processInstanceNo") String processInstanceNo, @Param("nodeId") String nodeId);
    
    List<ProcessNodeInstance> findNodeInstances(QueryProcessNodeReq queryReq);
    
    ProcessNodeInstance selectByNodeInstanceNo(String nodeInstanceNo);
    
    List<ProcessNodeInstance> findActiveNodeInstances(String processInstanceNo);
    
    List<ProcessNodeInstance> findOpenNodeInstances(String processInstanceNo);
    
    List<ProcessNodeInstance> queryNodeInstanceByNos(@Param("nodeInstanceNos") List<String> nodeInstanceNos);

}