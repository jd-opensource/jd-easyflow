package com.jd.easyflow.processunit.infrastructure.persistence.mapper;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.processunit.domain.model.vo.ProcessUnitInstanceKey;
import com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitInstance;

/**
 * @author liyuliang5
 * 
 */
@Mapper
public interface ProcessUnitInstanceMapper {

    int insert(ProcessUnitInstance record);

    ProcessUnitInstance selectByPrimaryKey(Long id);

    int updateByPrimaryKey(ProcessUnitInstance record);
    
    int updateByInstanceNo(ProcessUnitInstance record);
    
    int updateByInstanceNoSelective(ProcessUnitInstance record);

    ProcessUnitInstance getByUnitCodeAndBizNo(@Param("unitCode") String unitCode, @Param("bizNo") String bizNo);

    ProcessUnitInstance getByInstanceNo(String instanceNo);
    
    List<ProcessUnitInstance> selectListByUnitCodeAndBizNoPrefix(@Param("unitCode") String unitCode, @Param("bizNoPrefix") String bizNoPrefix);

    List<String> findAsyncInstanceList(QueryAsyncInstanceVO vo);
    
    List<ProcessUnitInstanceKey> findAsyncInstanceKeyList(QueryAsyncInstanceVO vo);

    List<ProcessUnitInstance> selectProcessUnitInstanceByPageCondition(PagerCondition pagerQueryReq);

    long countProcessUnitInstanceByPageCondition(PagerCondition pagerQueryReq);

    List<ProcessUnitInstance> selectProcessUnitInstance(@Param("createdDate") Date createdDate, @Param("result") String result);

    int physicsDeleteProcessUnitInstance(@Param("ids") Collection<Long> ids);
    
    int insertForMigration(ProcessUnitInstance record);

    int updateForMigration(ProcessUnitInstance record);

}