 package com.jd.easyflow.processunit.domain.repository;

import java.util.List;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.domain.model.vo.ProcessUnitInstanceKey;
import com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO;
import com.jd.easyflow.sharding.service.ShardingComputeResult;

/**
  * @author liyuliang5
  * 
  */
 public interface ProcessUnitRepository {
     
     ProcessUnitEntity getProcessUnitByCode(String processUnitCode);
     
     ProcessUnitInstanceEntity getInstanceByUnitCodeAndBizNo(String unitCode, String bizNo);
     
     List<ProcessUnitInstanceEntity> queryByUnitCodeAndBizNoPrefix(String unitCode, String bizNo);
     
     ProcessUnitInstanceEntity getInstance(String instanceNo);
     
     ProcessUnitInstanceEntity getInstance(String instanceNo, String unitCode, String bizNo);
     
     void saveInstance(ProcessUnitInstanceEntity instance);
     
     void updateInstance(ProcessUnitInstanceEntity instance);
     
     void updateInstanceByInstanceNoSelective(ProcessUnitInstanceEntity instance);
     
     ProcessUnitExecutionEntity getExecution(String executionNo);
     
     ProcessUnitExecutionEntity getExecution(String executionNo, String unitCode, String bizNo);
     
     void saveExecution(ProcessUnitExecutionEntity execution);
     
     void updateExecution(ProcessUnitExecutionEntity execution);

    void updateExecutionByExecutionNo(ProcessUnitExecutionEntity execution);
     
     List<String> findAsyncInstanceList(QueryAsyncInstanceVO query);
     
     List<ProcessUnitInstanceKey> findAsyncInstanceKeyList(QueryAsyncInstanceVO query);
     
     List<ProcessUnitEntity> findAllProcessUnitList();
     
     PagerResult pagerQueryProcessUnitInstance(PagerCondition pagerQueryReq);
     
     PagerResult pagerQueryProcessUnitExecution(PagerCondition pagerQueryReq);
     
     ShardingComputeResult computeShardInfo(String unitCode, String bizNo);
     
}
