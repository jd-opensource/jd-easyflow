package com.jd.easyflow.process.infrastructure.persistence.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.process.domain.model.vo.QueryTaskAssignReqVO;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTaskAssign;
import com.jd.easyflow.sharding.mybatis.ShardingSupportedMapper;

/**
 * 
 * @author liyuliang5
 *
 */
@ShardingSupportedMapper
public interface ProcessTaskAssignMapper {
    
    int deleteByPrimaryKey(Long id);

    int insert(ProcessTaskAssign record);
    
    int insertWithCreatedDate(ProcessTaskAssign record);

    ProcessTaskAssign selectByPrimaryKey(Long id);
    
    List<ProcessTaskAssign> list(QueryTaskAssignReqVO vo);
    
    List<ProcessTaskAssign> listByTaskNoList(@Param("taskNoList") List<String> taskNoList);

    int updateByPrimaryKeySelective(ProcessTaskAssign record);

    int updateByPrimaryKey(ProcessTaskAssign record);
    
    int updateByTaskAssignNo(ProcessTaskAssign record);
    
    int deleteTaskAssignByAssignNo(String assignNo);
}