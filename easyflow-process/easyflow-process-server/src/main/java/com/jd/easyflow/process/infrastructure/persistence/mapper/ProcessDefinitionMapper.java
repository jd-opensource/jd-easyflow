package com.jd.easyflow.process.infrastructure.persistence.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessDefinition;

/**
 * @author liyuliang5
 *
 */
public interface ProcessDefinitionMapper {


    int insert(ProcessDefinition record);


    ProcessDefinition selectByPrimaryKey(Long id);

    ProcessDefinition selectByDefIdAndVersion(@Param("defId") String defId, @Param("defVersion")Integer defVersion);

    int updateByPrimaryKeySelective(ProcessDefinition record);

    int updateProcessDefinitionLatestStatus(long id);

    long countProcessDefByPageCondition(PagerCondition pagerQueryReq);

    List<ProcessDefinition> selectProcessDefByPageCondition(PagerCondition pagerQueryReq);

    ProcessDefinition findLatestDefinition(String defId);

    boolean existProcessDefinition(String defId);

}