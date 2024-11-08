package com.jd.easyflow.process.infrastructure.persistence.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.process.domain.model.vo.QueryTaskAssignReqVO;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTaskAssign;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ProcessTaskAssignMapper {
    
    int deleteByPrimaryKey(Long id);

    int insert(ProcessTaskAssign record);
    
    int insertWithCreatedDate(ProcessTaskAssign record);

    int insertSelective(ProcessTaskAssign record);

    ProcessTaskAssign selectByPrimaryKey(Long id);
    
    List<ProcessTaskAssign> list(QueryTaskAssignReqVO vo);
    
    List<ProcessTaskAssign> listByTaskNoList(@Param("taskNoList") List<String> taskNoList);

    int updateByPrimaryKeySelective(ProcessTaskAssign record);

    int updateByPrimaryKey(ProcessTaskAssign record);
    
    int updateByTaskAssignNo(ProcessTaskAssign record);
}