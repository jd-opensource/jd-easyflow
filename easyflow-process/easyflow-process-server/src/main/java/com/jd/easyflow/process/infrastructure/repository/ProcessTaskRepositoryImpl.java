package com.jd.easyflow.process.infrastructure.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.process.domain.constant.ProcessConstants;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskAssignEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEventEntity;
import com.jd.easyflow.process.domain.model.vo.QueryTaskAssignReqVO;
import com.jd.easyflow.process.domain.model.vo.QueryTaskEventReqVO;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.TaskInfoForPagerVO;
import com.jd.easyflow.process.domain.repository.ProcessTaskRepository;
import com.jd.easyflow.process.infrastructure.converter.ProcessTaskConverter;
import com.jd.easyflow.process.infrastructure.persistence.mapper.ProcessTaskAssignMapper;
import com.jd.easyflow.process.infrastructure.persistence.mapper.ProcessTaskEventMapper;
import com.jd.easyflow.process.infrastructure.persistence.mapper.ProcessTaskMapper;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTask;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTaskAssign;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTaskEvent;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskRepositoryImpl implements ProcessTaskRepository {

    @Autowired
    private ProcessTaskMapper processTaskMapper;
    @Autowired
    private ProcessTaskAssignMapper processTaskAssignMapper;
    @Autowired
    private ProcessTaskEventMapper processTaskEventMapper;

    @Override
    public List<ProcessTaskEntity> queryTask(QueryTaskReqVO query) {
        List<ProcessTask> list = processTaskMapper.list(query);
        return ProcessTaskConverter.INSTANCE.convert(list);
    }

    @Transactional(transactionManager = ProcessConstants.BEAN_TX_MANAGER)
    @Override
    public void saveOrUpdate(ProcessTaskEntity taskEntity, List<ProcessTaskEventEntity> eventEntityList,
            List<ProcessTaskAssignEntity> assignEntityList) {
        if (taskEntity != null) {
            ProcessTask processTask = ProcessTaskConverter.INSTANCE.convert(taskEntity);
            if (processTask.getId() == null) {
                processTaskMapper.insert(processTask);
                taskEntity.setId(processTask.getId());
            } else {
                processTaskMapper.updateByPrimaryKey(processTask);
            }
        }

        if (assignEntityList != null) {
            for (ProcessTaskAssignEntity assignEntity : assignEntityList) {
                ProcessTaskAssign assign = ProcessTaskConverter.INSTANCE.convert(assignEntity);
                if (assign.getId() == null) {
                    processTaskAssignMapper.insert(assign);
                    assignEntity.setId(assign.getId());
                } else {
                    processTaskAssignMapper.updateByPrimaryKey(assign);
                }
            }
        }

        if (eventEntityList != null) {
            for (ProcessTaskEventEntity eventEntity : eventEntityList) {
                ProcessTaskEvent event = ProcessTaskConverter.INSTANCE.convert(eventEntity);
                if (event.getId() == null) {
                    processTaskEventMapper.insert(event);
                    eventEntity.setId(event.getId());
                } else {
                    processTaskEventMapper.updateByPrimaryKey(event);
                }
            }
        }

    }

    @Override
    public ProcessTaskEntity getTask(String taskNo) {
        QueryTaskReqVO query = new QueryTaskReqVO();
        query.setTaskNo(taskNo);
        List<ProcessTask> list = processTaskMapper.list(query);
        if (list.size() == 0) {
            return null;
        }
        if (list.size() > 1) {
            throw new IllegalStateException("result size > 1, list:" + list);
        }
        return ProcessTaskConverter.INSTANCE.convert(list.get(0));
    }

    @Override
    public List<ProcessTaskAssignEntity> findTaskAssignListByTaskNo(String taskNo) {
        AssertUtils.isNotNull(taskNo, "Task no can not be null");
        QueryTaskAssignReqVO query = new QueryTaskAssignReqVO();
        query.setTaskNo(taskNo);
        List<ProcessTaskAssign> list = processTaskAssignMapper.list(query);
        return ProcessTaskConverter.INSTANCE.convertAssignPoList(list);
    }

    /**
     * Pager query process task. condition:
     * taskNo, processType, bizNo, productCode, executor
     * createdDateStart, createdDateEnd, taskStatus
     * assignUser, assignGroupList:, assignGroup2List,
     * assignStatus
     * 
     */
    @Override
    public PagerResult<TaskInfoForPagerVO> pagerQueryTask(PagerCondition condition) {
        FieldEntry assignUserEntry = condition.getField("assignUser");
        FieldEntry assignGroupListEntry = condition.getField("assignGroupList");
        FieldEntry assignGroup2ListEntry = condition.getField("assignGroup2List");
        long count = -1;
        List<ProcessTask> list = null;
        if (assignUserEntry == null && assignGroupListEntry == null && assignGroup2ListEntry == null) {
            count = processTaskMapper.countTaskByPagerCondition(condition);
            list = processTaskMapper.selectTaskByPagerCondition(condition);
        } else {
            count = processTaskMapper.countTaskAndAssignByPagerCondition(condition);
            List<ProcessTask> taskNoList = processTaskMapper.selectTaskAndAssignByPagerCondition(condition);
            if (taskNoList.size() == 0) {
                list = new ArrayList<>();
            } else {
                PagerCondition taskCondition = new PagerCondition(1, taskNoList.size());
                taskCondition.addField(new FieldEntry("taskNoList", taskNoList.stream().map(task -> task.getTaskNo()).collect(Collectors.toList())));
                List<ProcessTask> noOrderList = processTaskMapper.selectTaskByPagerCondition(taskCondition);
                list = new ArrayList<>();
                for (ProcessTask taskNo : taskNoList) {
                    for (ProcessTask task : noOrderList) {
                        if (Objects.equals(taskNo.getTaskNo(), task.getTaskNo())) {
                            list.add(task);
                            break;
                        }
                    }
                }

            }
        }
        List<TaskInfoForPagerVO> voList = ProcessTaskConverter.INSTANCE.convertPagerList(list);
        return new PagerResult<>(count, voList);
    }

    @Override
    public List<ProcessTaskAssignEntity> findTaskAssignListByTaskNoList(List<String> taskNoList) {
        List<ProcessTaskAssign> assignList = processTaskAssignMapper.listByTaskNoList(taskNoList);
        return ProcessTaskConverter.INSTANCE.convertAssignPoList(assignList);
    }

    @Override
    public void save(ProcessTaskEntity taskEntity) {
        ProcessTask processTask = ProcessTaskConverter.INSTANCE.convert(taskEntity);
        processTaskMapper.insert(processTask);
        taskEntity.setId(processTask.getId());

    }

    @Override
    public void updateById(ProcessTaskEntity taskEntity) {
        ProcessTask processTask = ProcessTaskConverter.INSTANCE.convert(taskEntity);
        processTaskMapper.updateByPrimaryKey(processTask);
    }
    
    @Override
    public void updateByTaskNo(ProcessTaskEntity taskEntity) {
        ProcessTask processTask = ProcessTaskConverter.INSTANCE.convert(taskEntity);
        processTaskMapper.updateByTaskNo(processTask);
    }

    @Override
    public void save(ProcessTaskAssignEntity assignEntity) {
        ProcessTaskAssign assign = ProcessTaskConverter.INSTANCE.convert(assignEntity);
        processTaskAssignMapper.insert(assign);
        assignEntity.setId(assign.getId());
    }

    @Override
    public void updateById(ProcessTaskAssignEntity assignEntity) {
        ProcessTaskAssign assign = ProcessTaskConverter.INSTANCE.convert(assignEntity);
        processTaskAssignMapper.updateByPrimaryKey(assign);
    }
    
    @Override
    public void updateByTaskAssignNo(ProcessTaskAssignEntity assignEntity) {
        ProcessTaskAssign assign = ProcessTaskConverter.INSTANCE.convert(assignEntity);
        processTaskAssignMapper.updateByTaskAssignNo(assign);
    }

    @Override
    public void save(ProcessTaskEventEntity eventEntity) {
        ProcessTaskEvent event = ProcessTaskConverter.INSTANCE.convert(eventEntity);
        processTaskEventMapper.insert(event);
        eventEntity.setId(event.getId());

    }

    @Override
    public ProcessTaskAssignEntity getTaskAssign(String taskAssignNo) {
        QueryTaskAssignReqVO req = new QueryTaskAssignReqVO();
        req.setAssignNo(taskAssignNo);
        List<ProcessTaskAssign> list = processTaskAssignMapper.list(req);
        if (list.size() == 0) {
            return null;
        }
        if (list.size() > 1) {
            throw new IllegalStateException("result size > 1, list:" + list);
        }
        return ProcessTaskConverter.INSTANCE.convert(list.get(0));
    }

    @Override
    public ProcessTaskEventEntity getTaskEvent(String taskEventNo) {
        QueryTaskEventReqVO req = new QueryTaskEventReqVO();
        req.setEventNo(taskEventNo);
        List<ProcessTaskEvent> list = processTaskEventMapper.list(req);
        if (list.size() == 0) {
            return null;
        }
        if (list.size() > 1) {
            throw new IllegalStateException("result size > 1, list:" + list);
        }
        return ProcessTaskConverter.INSTANCE.convert(list.get(0));
    }

    @Override
    public void updateById(ProcessTaskEventEntity eventEntity) {
        ProcessTaskEvent processTaskEvent = ProcessTaskConverter.INSTANCE.convert(eventEntity);
        processTaskEventMapper.updateByPrimaryKey(processTaskEvent);
    }
    
    @Override
    public void updateByTaskEventNo(ProcessTaskEventEntity eventEntity) {
        ProcessTaskEvent processTaskEvent = ProcessTaskConverter.INSTANCE.convert(eventEntity);
        processTaskEventMapper.updateByTaskEventNo(processTaskEvent);
    }

    @Override
    public List<ProcessTaskEventEntity> findTaskEventListByTaskNo(String taskNo) {
        QueryTaskEventReqVO query = new QueryTaskEventReqVO();
        query.setTaskNo(taskNo);
        List<ProcessTaskEvent> list = processTaskEventMapper.list(query);
        return ProcessTaskConverter.INSTANCE.convertEventPoList(list);
    }

    @Override
    public void saveWithCreatedDate(ProcessTaskEntity taskEntity) {
        ProcessTask processTask = ProcessTaskConverter.INSTANCE.convert(taskEntity);
        processTaskMapper.insertWithCreatedDate(processTask);
        taskEntity.setId(processTask.getId());
    }

    @Override
    public void saveWithCreatedDate(ProcessTaskAssignEntity assignEntity) {
        ProcessTaskAssign assign = ProcessTaskConverter.INSTANCE.convert(assignEntity);
        processTaskAssignMapper.insertWithCreatedDate(assign);
        assignEntity.setId(assign.getId());
    }

    @Override
    public void saveWithCreatedDate(ProcessTaskEventEntity eventEntity) {
        ProcessTaskEvent event = ProcessTaskConverter.INSTANCE.convert(eventEntity);
        processTaskEventMapper.insertWithCreatedDate(event);
        eventEntity.setId(event.getId());
        
    }

}
