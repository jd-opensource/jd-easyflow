package com.jd.easyflow.process.infrastructure.converter;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.process.domain.model.entity.ProcessTaskAssignEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEventEntity;
import com.jd.easyflow.process.domain.model.vo.TaskInfoForPagerVO;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTask;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTaskAssign;
import com.jd.easyflow.process.infrastructure.persistence.po.ProcessTaskEvent;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskConverter {

    public static ProcessTaskConverter INSTANCE = new ProcessTaskConverter();
    
    public List<ProcessTaskEntity> convert(List<ProcessTask> list) {
        if ( list == null ) {
            return null;
        }

        List<ProcessTaskEntity> list1 = new ArrayList<ProcessTaskEntity>( list.size() );
        for ( ProcessTask processTask : list ) {
            list1.add( convert( processTask ) );
        }

        return list1;
    }

    public ProcessTask convert(ProcessTaskEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessTask processTask = new ProcessTask();

        processTask.setAssignInfo( entity.getAssignInfo() );
        processTask.setAssignTime( entity.getAssignTime() );
        processTask.setAssignType( entity.getAssignType() );
        processTask.setBizNo( entity.getBizNo() );
        processTask.setCreatedDate( entity.getCreatedDate() );
        processTask.setCreator( entity.getCreator() );
        processTask.setExecuteBizData( entity.getExecuteBizData() );
        processTask.setExecuteBizResult( entity.getExecuteBizResult() );
        processTask.setExecuteTime( entity.getExecuteTime() );
        processTask.setExecutor( entity.getExecutor() );
        processTask.setExtData( entity.getExtData() );
        processTask.setId( entity.getId() );
        processTask.setModifiedDate( entity.getModifiedDate() );
        processTask.setNodeExecutionNo( entity.getNodeExecutionNo() );
        processTask.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processTask.setProcessInstanceKeyField( entity.getProcessInstanceKeyField() );
        processTask.setProcessInstanceKeyField2( entity.getProcessInstanceKeyField2() );
        processTask.setProcessInstanceNo( entity.getProcessInstanceNo() );
        processTask.setProcessType( entity.getProcessType() );
        processTask.setProductCode( entity.getProductCode() );
        processTask.setStatus( entity.getStatus() );
        processTask.setTaskBizCode( entity.getTaskBizCode() );
        processTask.setTaskBizName( entity.getTaskBizName() );
        processTask.setTaskNo( entity.getTaskNo() );
        processTask.setTaskType( entity.getTaskType() );

        return processTask;
    }

    public ProcessTaskEntity convert(ProcessTask entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessTaskEntity processTaskEntity = new ProcessTaskEntity();

        processTaskEntity.setAssignInfo( entity.getAssignInfo() );
        processTaskEntity.setAssignTime( entity.getAssignTime() );
        processTaskEntity.setAssignType( entity.getAssignType() );
        processTaskEntity.setBizNo( entity.getBizNo() );
        processTaskEntity.setCreatedDate( entity.getCreatedDate() );
        processTaskEntity.setCreator( entity.getCreator() );
        processTaskEntity.setExecuteBizData( entity.getExecuteBizData() );
        processTaskEntity.setExecuteBizResult( entity.getExecuteBizResult() );
        processTaskEntity.setExecuteTime( entity.getExecuteTime() );
        processTaskEntity.setExecutor( entity.getExecutor() );
        processTaskEntity.setExtData( entity.getExtData() );
        processTaskEntity.setId( entity.getId() );
        processTaskEntity.setModifiedDate( entity.getModifiedDate() );
        processTaskEntity.setNodeExecutionNo( entity.getNodeExecutionNo() );
        processTaskEntity.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processTaskEntity.setProcessInstanceKeyField( entity.getProcessInstanceKeyField() );
        processTaskEntity.setProcessInstanceKeyField2( entity.getProcessInstanceKeyField2() );
        processTaskEntity.setProcessInstanceNo( entity.getProcessInstanceNo() );
        processTaskEntity.setProcessType( entity.getProcessType() );
        processTaskEntity.setProductCode( entity.getProductCode() );
        processTaskEntity.setStatus( entity.getStatus() );
        processTaskEntity.setTaskBizCode( entity.getTaskBizCode() );
        processTaskEntity.setTaskBizName( entity.getTaskBizName() );
        processTaskEntity.setTaskNo( entity.getTaskNo() );
        processTaskEntity.setTaskType( entity.getTaskType() );

        return processTaskEntity;
    }

    public ProcessTaskAssign convert(ProcessTaskAssignEntity assignEntity) {
        if ( assignEntity == null ) {
            return null;
        }

        ProcessTaskAssign processTaskAssign = new ProcessTaskAssign();

        processTaskAssign.setAssignGroup( assignEntity.getAssignGroup() );
        processTaskAssign.setAssignGroup2( assignEntity.getAssignGroup2() );
        processTaskAssign.setAssignNo( assignEntity.getAssignNo() );
        processTaskAssign.setAssignTime( assignEntity.getAssignTime() );
        processTaskAssign.setAssignType( assignEntity.getAssignType() );
        processTaskAssign.setAssignUser( assignEntity.getAssignUser() );
        processTaskAssign.setCreatedDate( assignEntity.getCreatedDate() );
        processTaskAssign.setExtData( assignEntity.getExtData() );
        processTaskAssign.setId( assignEntity.getId() );
        processTaskAssign.setModifiedDate( assignEntity.getModifiedDate() );
        processTaskAssign.setProductCode( assignEntity.getProductCode() );
        processTaskAssign.setStatus( assignEntity.getStatus() );
        processTaskAssign.setTaskNo( assignEntity.getTaskNo() );

        return processTaskAssign;
    }

    public ProcessTaskAssignEntity convert(ProcessTaskAssign assign) {
        if ( assign == null ) {
            return null;
        }

        ProcessTaskAssignEntity processTaskAssignEntity = new ProcessTaskAssignEntity();

        processTaskAssignEntity.setAssignGroup( assign.getAssignGroup() );
        processTaskAssignEntity.setAssignGroup2( assign.getAssignGroup2() );
        processTaskAssignEntity.setAssignNo( assign.getAssignNo() );
        processTaskAssignEntity.setAssignTime( assign.getAssignTime() );
        processTaskAssignEntity.setAssignType( assign.getAssignType() );
        processTaskAssignEntity.setAssignUser( assign.getAssignUser() );
        processTaskAssignEntity.setCreatedDate( assign.getCreatedDate() );
        processTaskAssignEntity.setExtData( assign.getExtData() );
        processTaskAssignEntity.setId( assign.getId() );
        processTaskAssignEntity.setModifiedDate( assign.getModifiedDate() );
        processTaskAssignEntity.setProductCode( assign.getProductCode() );
        processTaskAssignEntity.setStatus( assign.getStatus() );
        processTaskAssignEntity.setTaskNo( assign.getTaskNo() );

        return processTaskAssignEntity;
    }

    public List<ProcessTaskAssign> convertAssignList(List<ProcessTaskAssignEntity> entityList) {
        if ( entityList == null ) {
            return null;
        }

        List<ProcessTaskAssign> list = new ArrayList<ProcessTaskAssign>( entityList.size() );
        for ( ProcessTaskAssignEntity processTaskAssignEntity : entityList ) {
            list.add( convert( processTaskAssignEntity ) );
        }

        return list;
    }

    public List<ProcessTaskAssignEntity> convertAssignPoList(List<ProcessTaskAssign> assignList) {
        if ( assignList == null ) {
            return null;
        }

        List<ProcessTaskAssignEntity> list = new ArrayList<ProcessTaskAssignEntity>( assignList.size() );
        for ( ProcessTaskAssign processTaskAssign : assignList ) {
            list.add( convert( processTaskAssign ) );
        }

        return list;
    }

    public ProcessTaskEvent convert(ProcessTaskEventEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessTaskEvent processTaskEvent = new ProcessTaskEvent();

        processTaskEvent.setCreatedDate( entity.getCreatedDate() );
        processTaskEvent.setEventBizData( entity.getEventBizData() );
        processTaskEvent.setEventBizResult( entity.getEventBizResult() );
        processTaskEvent.setEventNo( entity.getEventNo() );
        processTaskEvent.setEventTime( entity.getEventTime() );
        processTaskEvent.setEventType( entity.getEventType() );
        processTaskEvent.setEventUser( entity.getEventUser() );
        processTaskEvent.setExtData( entity.getExtData() );
        processTaskEvent.setId( entity.getId() );
        processTaskEvent.setInstanceBizData( entity.getInstanceBizData() );
        processTaskEvent.setInstanceBizStatus( entity.getInstanceBizStatus() );
        processTaskEvent.setModifiedDate( entity.getModifiedDate() );
        processTaskEvent.setProductCode( entity.getProductCode() );
        processTaskEvent.setTaskNo( entity.getTaskNo() );

        return processTaskEvent;
    }

    public ProcessTaskEventEntity convert(ProcessTaskEvent event) {
        if ( event == null ) {
            return null;
        }

        ProcessTaskEventEntity processTaskEventEntity = new ProcessTaskEventEntity();

        processTaskEventEntity.setCreatedDate( event.getCreatedDate() );
        processTaskEventEntity.setEventBizData( event.getEventBizData() );
        processTaskEventEntity.setEventBizResult( event.getEventBizResult() );
        processTaskEventEntity.setEventNo( event.getEventNo() );
        processTaskEventEntity.setEventTime( event.getEventTime() );
        processTaskEventEntity.setEventType( event.getEventType() );
        processTaskEventEntity.setEventUser( event.getEventUser() );
        processTaskEventEntity.setExtData( event.getExtData() );
        processTaskEventEntity.setId( event.getId() );
        processTaskEventEntity.setInstanceBizData( event.getInstanceBizData() );
        processTaskEventEntity.setInstanceBizStatus( event.getInstanceBizStatus() );
        processTaskEventEntity.setModifiedDate( event.getModifiedDate() );
        processTaskEventEntity.setProductCode( event.getProductCode() );
        processTaskEventEntity.setTaskNo( event.getTaskNo() );

        return processTaskEventEntity;
    }

    public List<ProcessTaskEvent> convertEventEntityList(List<ProcessTaskEventEntity> entityList) {
        if ( entityList == null ) {
            return null;
        }

        List<ProcessTaskEvent> list = new ArrayList<ProcessTaskEvent>( entityList.size() );
        for ( ProcessTaskEventEntity processTaskEventEntity : entityList ) {
            list.add( convert( processTaskEventEntity ) );
        }

        return list;
    }

    public List<ProcessTaskEventEntity> convertEventPoList(List<ProcessTaskEvent> eventList) {
        if ( eventList == null ) {
            return null;
        }

        List<ProcessTaskEventEntity> list = new ArrayList<ProcessTaskEventEntity>( eventList.size() );
        for ( ProcessTaskEvent processTaskEvent : eventList ) {
            list.add( convert( processTaskEvent ) );
        }

        return list;
    }

    public List<TaskInfoForPagerVO> convertPagerList(List<ProcessTask> list) {
        if ( list == null ) {
            return null;
        }

        List<TaskInfoForPagerVO> list1 = new ArrayList<TaskInfoForPagerVO>( list.size() );
        for ( ProcessTask processTask : list ) {
            list1.add( processTaskToTaskInfoForPagerVO( processTask ) );
        }

        return list1;
    }

    protected TaskInfoForPagerVO processTaskToTaskInfoForPagerVO(ProcessTask processTask) {
        if ( processTask == null ) {
            return null;
        }

        TaskInfoForPagerVO taskInfoForPagerVO = new TaskInfoForPagerVO();

        taskInfoForPagerVO.setBizNo( processTask.getBizNo() );
        if ( processTask.getCreatedDate() != null ) {
            taskInfoForPagerVO.setCreatedDate( new SimpleDateFormat().format( processTask.getCreatedDate() ) );
        }
        taskInfoForPagerVO.setCreator( processTask.getCreator() );
        taskInfoForPagerVO.setExecuteBizData( processTask.getExecuteBizData() );
        taskInfoForPagerVO.setExecuteBizResult( processTask.getExecuteBizResult() );
        if ( processTask.getExecuteTime() != null ) {
            taskInfoForPagerVO.setExecuteTime( new SimpleDateFormat().format( processTask.getExecuteTime() ) );
        }
        taskInfoForPagerVO.setExecutor( processTask.getExecutor() );
        if ( processTask.getModifiedDate() != null ) {
            taskInfoForPagerVO.setModifiedDate( new SimpleDateFormat().format( processTask.getModifiedDate() ) );
        }
        taskInfoForPagerVO.setNodeInstanceNo( processTask.getNodeInstanceNo() );
        taskInfoForPagerVO.setProcessInstanceNo( processTask.getProcessInstanceNo() );
        taskInfoForPagerVO.setProcessType( processTask.getProcessType() );
        taskInfoForPagerVO.setProductCode( processTask.getProductCode() );
        taskInfoForPagerVO.setStatus( processTask.getStatus() );
        taskInfoForPagerVO.setTaskBizCode( processTask.getTaskBizCode() );
        taskInfoForPagerVO.setTaskBizName( processTask.getTaskBizName() );
        taskInfoForPagerVO.setTaskNo( processTask.getTaskNo() );

        return taskInfoForPagerVO;
    }
}
