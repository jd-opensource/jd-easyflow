package com.jd.easyflow.process.domain.repository;

import java.util.List;

import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskAssignEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEventEntity;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.TaskInfoForPagerVO;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ProcessTaskRepository {

    
    void saveOrUpdate(ProcessTaskEntity taskEntity, List<ProcessTaskEventEntity> eventList, List<ProcessTaskAssignEntity> assignEntityList);
    
    void save(ProcessTaskEntity taskEntity);
    
    void saveWithCreatedDate(ProcessTaskEntity taskEntity);
    
    void updateById(ProcessTaskEntity taskEntity);
    
    void updateByTaskNo(ProcessTaskEntity taskEntity);
    
    void save(ProcessTaskAssignEntity assignEntity);
    
    void saveWithCreatedDate(ProcessTaskAssignEntity assignEntity);
    
    void updateById(ProcessTaskAssignEntity assignEntity);
    
    void updateByTaskAssignNo(ProcessTaskAssignEntity assignEntity);
    
    void save(ProcessTaskEventEntity eventEntity);
    
    void saveWithCreatedDate(ProcessTaskEventEntity eventEntity);
    
    void updateById(ProcessTaskEventEntity eventEntity);
    
    void updateByTaskEventNo(ProcessTaskEventEntity eventEntity);
    
    void deleteTaskByTaskNo(String taskNo);
    
    void deleteTaskAssignByAssignNo(String assignNo);
    
    
    

    List<ProcessTaskEntity> queryTask(QueryTaskReqVO query);
    
    ProcessTaskEntity getTask(String taskNo);
    
    ProcessTaskAssignEntity getTaskAssign(String taskAssignNo);
    
    ProcessTaskEventEntity getTaskEvent(String taskEventNo);

    List<ProcessTaskAssignEntity> findTaskAssignListByTaskNo(String taskNo);
    
    List<ProcessTaskAssignEntity> findTaskAssignListByTaskNoList(List<String> taskNoList);
    
    List<ProcessTaskEventEntity> findTaskEventListByTaskNo(String taskNo);
    
    PagerResult<TaskInfoForPagerVO> pagerQueryTask(PagerCondition condition);

}
