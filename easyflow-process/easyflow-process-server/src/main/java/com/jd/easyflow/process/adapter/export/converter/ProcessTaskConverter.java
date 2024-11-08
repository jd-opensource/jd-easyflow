package com.jd.easyflow.process.adapter.export.converter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.jd.easyflow.process.adapter.export.dto.task.ExecuteTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskEventDTO;
import com.jd.easyflow.process.adapter.export.dto.task.QueryTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCmd;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCommand;
import com.jd.easyflow.process.adapter.export.dto.task.command.TaskCreateCommand;
import com.jd.easyflow.process.adapter.export.dto.task.command.TaskExecuteCommand;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskAssignEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEventEntity;
import com.jd.easyflow.process.domain.model.vo.CreateProcessTaskVO;
import com.jd.easyflow.process.domain.model.vo.DoExecuteProcessTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.ExecuteProcessTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.TaskInfoForPagerVO;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskConverter {

    public static ProcessTaskConverter INSTANCE = new ProcessTaskConverter();

    public ExecuteProcessTaskReqVO convert(ExecuteTaskReq req) {
        if ( req == null ) {
            return null;
        }

        ExecuteProcessTaskReqVO executeProcessTaskReqVO = new ExecuteProcessTaskReqVO();

        List<TaskOperateCmd> list = req.getCmdList();
        if ( list != null ) {
            executeProcessTaskReqVO.setCmdList( new ArrayList<TaskOperateCmd>( list ) );
        }
        executeProcessTaskReqVO.setExecuteBizData( req.getExecuteBizData() );
        executeProcessTaskReqVO.setExecuteBizResult( req.getExecuteBizResult() );
        List<String> list1 = req.getGroup2List();
        if ( list1 != null ) {
            executeProcessTaskReqVO.setGroup2List( new ArrayList<String>( list1 ) );
        }
        List<String> list2 = req.getGroupList();
        if ( list2 != null ) {
            executeProcessTaskReqVO.setGroupList( new ArrayList<String>( list2 ) );
        }
        executeProcessTaskReqVO.setInstanceBizData( req.getInstanceBizData() );
        executeProcessTaskReqVO.setInstanceBizStatus( req.getInstanceBizStatus() );
        executeProcessTaskReqVO.setOperation( req.getOperation() );
        executeProcessTaskReqVO.setTaskExtData( req.getTaskExtData() );
        executeProcessTaskReqVO.setTaskNo( req.getTaskNo() );
        executeProcessTaskReqVO.setUser( req.getUser() );

        return executeProcessTaskReqVO;
    }

    public List<ProcessTaskDTO> convert(List<TaskInfoForPagerVO> list) {
        if ( list == null ) {
            return null;
        }

        List<ProcessTaskDTO> list1 = new ArrayList<ProcessTaskDTO>( list.size() );
        for ( TaskInfoForPagerVO taskInfoForPagerVO : list ) {
            try {
                list1.add( taskInfoForPagerVOToProcessTaskDTO( taskInfoForPagerVO ) );
            }
            catch ( ParseException e ) {
                throw new RuntimeException( e );
            }
        }

        return list1;
    }

    public ProcessTaskDTO convert(ProcessTaskEntity entity) {
        if ( entity == null ) {
            return null;
        }

        ProcessTaskDTO processTaskDTO = new ProcessTaskDTO();

        processTaskDTO.setAssignInfo( entity.getAssignInfo() );
        processTaskDTO.setAssignTime( entity.getAssignTime() );
        processTaskDTO.setAssignType( entity.getAssignType() );
        processTaskDTO.setBizNo( entity.getBizNo() );
        processTaskDTO.setCreatedDate( entity.getCreatedDate() );
        processTaskDTO.setCreator( entity.getCreator() );
        processTaskDTO.setExecuteBizData( entity.getExecuteBizData() );
        processTaskDTO.setExecuteBizResult( entity.getExecuteBizResult() );
        processTaskDTO.setExecuteTime( entity.getExecuteTime() );
        processTaskDTO.setExecutor( entity.getExecutor() );
        processTaskDTO.setExtData( entity.getExtData() );
        processTaskDTO.setModifiedDate( entity.getModifiedDate() );
        processTaskDTO.setNodeExecutionNo( entity.getNodeExecutionNo() );
        processTaskDTO.setNodeInstanceNo( entity.getNodeInstanceNo() );
        processTaskDTO.setProcessInstanceKeyField( entity.getProcessInstanceKeyField() );
        processTaskDTO.setProcessInstanceKeyField2( entity.getProcessInstanceKeyField2() );
        processTaskDTO.setProcessInstanceNo( entity.getProcessInstanceNo() );
        processTaskDTO.setProcessType( entity.getProcessType() );
        processTaskDTO.setProductCode( entity.getProductCode() );
        processTaskDTO.setStatus( entity.getStatus() );
        processTaskDTO.setTaskBizCode( entity.getTaskBizCode() );
        processTaskDTO.setTaskBizName( entity.getTaskBizName() );
        processTaskDTO.setTaskNo( entity.getTaskNo() );
        processTaskDTO.setTaskType( entity.getTaskType() );

        return processTaskDTO;
    }

    public ProcessTaskEntity convert(ProcessTaskDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessTaskEntity processTaskEntity = new ProcessTaskEntity();

        processTaskEntity.setAssignInfo( dto.getAssignInfo() );
        processTaskEntity.setAssignTime( dto.getAssignTime() );
        processTaskEntity.setAssignType( dto.getAssignType() );
        processTaskEntity.setBizNo( dto.getBizNo() );
        processTaskEntity.setCreatedDate( dto.getCreatedDate() );
        processTaskEntity.setCreator( dto.getCreator() );
        processTaskEntity.setExecuteBizData( dto.getExecuteBizData() );
        processTaskEntity.setExecuteBizResult( dto.getExecuteBizResult() );
        processTaskEntity.setExecuteTime( dto.getExecuteTime() );
        processTaskEntity.setExecutor( dto.getExecutor() );
        processTaskEntity.setExtData( dto.getExtData() );
        processTaskEntity.setModifiedDate( dto.getModifiedDate() );
        processTaskEntity.setNodeExecutionNo( dto.getNodeExecutionNo() );
        processTaskEntity.setNodeInstanceNo( dto.getNodeInstanceNo() );
        processTaskEntity.setProcessInstanceKeyField( dto.getProcessInstanceKeyField() );
        processTaskEntity.setProcessInstanceKeyField2( dto.getProcessInstanceKeyField2() );
        processTaskEntity.setProcessInstanceNo( dto.getProcessInstanceNo() );
        processTaskEntity.setProcessType( dto.getProcessType() );
        processTaskEntity.setProductCode( dto.getProductCode() );
        processTaskEntity.setStatus( dto.getStatus() );
        processTaskEntity.setTaskBizCode( dto.getTaskBizCode() );
        processTaskEntity.setTaskBizName( dto.getTaskBizName() );
        processTaskEntity.setTaskNo( dto.getTaskNo() );
        processTaskEntity.setTaskType( dto.getTaskType() );

        return processTaskEntity;
    }

    public ProcessTaskAssignEntity convert(ProcessTaskAssignDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessTaskAssignEntity processTaskAssignEntity = new ProcessTaskAssignEntity();

        processTaskAssignEntity.setAssignGroup( dto.getAssignGroup() );
        processTaskAssignEntity.setAssignGroup2( dto.getAssignGroup2() );
        processTaskAssignEntity.setAssignNo( dto.getAssignNo() );
        processTaskAssignEntity.setAssignTime( dto.getAssignTime() );
        processTaskAssignEntity.setAssignType( dto.getAssignType() );
        processTaskAssignEntity.setAssignUser( dto.getAssignUser() );
        processTaskAssignEntity.setCreatedDate( dto.getCreatedDate() );
        processTaskAssignEntity.setExtData( dto.getExtData() );
        processTaskAssignEntity.setModifiedDate( dto.getModifiedDate() );
        processTaskAssignEntity.setProductCode( dto.getProductCode() );
        processTaskAssignEntity.setStatus( dto.getStatus() );
        processTaskAssignEntity.setTaskNo( dto.getTaskNo() );

        return processTaskAssignEntity;
    }

    public ProcessTaskEventEntity convert(ProcessTaskEventDTO dto) {
        if ( dto == null ) {
            return null;
        }

        ProcessTaskEventEntity processTaskEventEntity = new ProcessTaskEventEntity();

        processTaskEventEntity.setCreatedDate( dto.getCreatedDate() );
        processTaskEventEntity.setEventBizData( dto.getEventBizData() );
        processTaskEventEntity.setEventBizResult( dto.getEventBizResult() );
        processTaskEventEntity.setEventNo( dto.getEventNo() );
        processTaskEventEntity.setEventTime( dto.getEventTime() );
        processTaskEventEntity.setEventType( dto.getEventType() );
        processTaskEventEntity.setEventUser( dto.getEventUser() );
        processTaskEventEntity.setExtData( dto.getExtData() );
        processTaskEventEntity.setInstanceBizData( dto.getInstanceBizData() );
        processTaskEventEntity.setInstanceBizStatus( dto.getInstanceBizStatus() );
        processTaskEventEntity.setModifiedDate( dto.getModifiedDate() );
        processTaskEventEntity.setProductCode( dto.getProductCode() );
        processTaskEventEntity.setTaskNo( dto.getTaskNo() );

        return processTaskEventEntity;
    }

    public List<ProcessTaskAssignDTO> convertAssignList(List<ProcessTaskAssignEntity> entity) {
        if ( entity == null ) {
            return null;
        }

        List<ProcessTaskAssignDTO> list = new ArrayList<ProcessTaskAssignDTO>( entity.size() );
        for ( ProcessTaskAssignEntity processTaskAssignEntity : entity ) {
            list.add( processTaskAssignEntityToProcessTaskAssignDTO( processTaskAssignEntity ) );
        }

        return list;
    }

    public QueryTaskReqVO convert(QueryTaskReq req) {
        if ( req == null ) {
            return null;
        }

        QueryTaskReqVO queryTaskReqVO = new QueryTaskReqVO();

        queryTaskReqVO.setBizNo( req.getBizNo() );
        queryTaskReqVO.setNodeInstanceNo( req.getNodeInstanceNo() );
        queryTaskReqVO.setProcessInstanceNo( req.getProcessInstanceNo() );
        queryTaskReqVO.setProcessType( req.getProcessType() );
        queryTaskReqVO.setStatus( req.getStatus() );
        queryTaskReqVO.setTaskBizCode( req.getTaskBizCode() );

        return queryTaskReqVO;
    }

    public List<ProcessTaskDTO> convertEntityList(List<ProcessTaskEntity> entity) {
        if ( entity == null ) {
            return null;
        }

        List<ProcessTaskDTO> list = new ArrayList<ProcessTaskDTO>( entity.size() );
        for ( ProcessTaskEntity processTaskEntity : entity ) {
            list.add( convert( processTaskEntity ) );
        }

        return list;
    }

    public CreateProcessTaskVO convert(TaskCreateCommand command) {
        if ( command == null ) {
            return null;
        }

        CreateProcessTaskVO createProcessTaskVO = new CreateProcessTaskVO();

        Map<String, Object> map = command.getAssignInfo();
        if ( map != null ) {
            createProcessTaskVO.setAssignInfo( new HashMap<String, Object>( map ) );
        }
        createProcessTaskVO.setAssignType( command.getAssignType() );
        createProcessTaskVO.setBizNo( command.getBizNo() );
        createProcessTaskVO.setCreator( command.getCreator() );
        createProcessTaskVO.setExtData( command.getExtData() );
        createProcessTaskVO.setNodeInstanceNo( command.getNodeInstanceNo() );
        createProcessTaskVO.setProcessInstanceNo( command.getProcessInstanceNo() );
        createProcessTaskVO.setProcessType( command.getProcessType() );
        createProcessTaskVO.setProductCode( command.getProductCode() );
        createProcessTaskVO.setTaskBizCode( command.getTaskBizCode() );
        createProcessTaskVO.setTaskBizName( command.getTaskBizName() );
        createProcessTaskVO.setTaskType( command.getTaskType() );

        return createProcessTaskVO;
    }

    public DoExecuteProcessTaskReqVO convert(TaskExecuteCommand command) {
        if ( command == null ) {
            return null;
        }

        DoExecuteProcessTaskReqVO doExecuteProcessTaskReqVO = new DoExecuteProcessTaskReqVO();

        doExecuteProcessTaskReqVO.setExecuteBizData( command.getExecuteBizData() );
        doExecuteProcessTaskReqVO.setExecuteBizResult( command.getExecuteBizResult() );
        doExecuteProcessTaskReqVO.setExecutor( command.getExecutor() );
        List<String> list = command.getGroup2List();
        if ( list != null ) {
            doExecuteProcessTaskReqVO.setGroup2List( new ArrayList<String>( list ) );
        }
        List<String> list1 = command.getGroupList();
        if ( list1 != null ) {
            doExecuteProcessTaskReqVO.setGroupList( new ArrayList<String>( list1 ) );
        }
        doExecuteProcessTaskReqVO.setInstanceBizData( command.getInstanceBizData() );
        doExecuteProcessTaskReqVO.setInstanceBizStatus( command.getInstanceBizStatus() );
        doExecuteProcessTaskReqVO.setOperation( command.getOperation() );
        List<TaskOperateCommand> list2 = command.getSubCommandList();
        if ( list2 != null ) {
            doExecuteProcessTaskReqVO.setSubCommandList( new ArrayList<TaskOperateCommand>( list2 ) );
        }
        doExecuteProcessTaskReqVO.setTaskExtData( command.getTaskExtData() );
        doExecuteProcessTaskReqVO.setTaskNo( command.getTaskNo() );

        return doExecuteProcessTaskReqVO;
    }

    protected ProcessTaskDTO taskInfoForPagerVOToProcessTaskDTO(TaskInfoForPagerVO taskInfoForPagerVO) throws ParseException {
        if ( taskInfoForPagerVO == null ) {
            return null;
        }

        ProcessTaskDTO processTaskDTO = new ProcessTaskDTO();

        processTaskDTO.setBizNo( taskInfoForPagerVO.getBizNo() );
        if ( taskInfoForPagerVO.getCreatedDate() != null ) {
            processTaskDTO.setCreatedDate( new SimpleDateFormat().parse( taskInfoForPagerVO.getCreatedDate() ) );
        }
        processTaskDTO.setCreator( taskInfoForPagerVO.getCreator() );
        processTaskDTO.setExecuteBizData( taskInfoForPagerVO.getExecuteBizData() );
        processTaskDTO.setExecuteBizResult( taskInfoForPagerVO.getExecuteBizResult() );
        if ( taskInfoForPagerVO.getExecuteTime() != null ) {
            processTaskDTO.setExecuteTime( new SimpleDateFormat().parse( taskInfoForPagerVO.getExecuteTime() ) );
        }
        processTaskDTO.setExecutor( taskInfoForPagerVO.getExecutor() );
        if ( taskInfoForPagerVO.getModifiedDate() != null ) {
            processTaskDTO.setModifiedDate( new SimpleDateFormat().parse( taskInfoForPagerVO.getModifiedDate() ) );
        }
        processTaskDTO.setNodeInstanceNo( taskInfoForPagerVO.getNodeInstanceNo() );
        processTaskDTO.setProcessInstanceNo( taskInfoForPagerVO.getProcessInstanceNo() );
        processTaskDTO.setProcessType( taskInfoForPagerVO.getProcessType() );
        processTaskDTO.setProductCode( taskInfoForPagerVO.getProductCode() );
        processTaskDTO.setStatus( taskInfoForPagerVO.getStatus() );
        processTaskDTO.setTaskBizCode( taskInfoForPagerVO.getTaskBizCode() );
        processTaskDTO.setTaskBizName( taskInfoForPagerVO.getTaskBizName() );
        processTaskDTO.setTaskNo( taskInfoForPagerVO.getTaskNo() );

        return processTaskDTO;
    }

    protected ProcessTaskAssignDTO processTaskAssignEntityToProcessTaskAssignDTO(ProcessTaskAssignEntity processTaskAssignEntity) {
        if ( processTaskAssignEntity == null ) {
            return null;
        }

        ProcessTaskAssignDTO processTaskAssignDTO = new ProcessTaskAssignDTO();

        processTaskAssignDTO.setAssignGroup( processTaskAssignEntity.getAssignGroup() );
        processTaskAssignDTO.setAssignGroup2( processTaskAssignEntity.getAssignGroup2() );
        processTaskAssignDTO.setAssignNo( processTaskAssignEntity.getAssignNo() );
        processTaskAssignDTO.setAssignTime( processTaskAssignEntity.getAssignTime() );
        processTaskAssignDTO.setAssignType( processTaskAssignEntity.getAssignType() );
        processTaskAssignDTO.setAssignUser( processTaskAssignEntity.getAssignUser() );
        processTaskAssignDTO.setCreatedDate( processTaskAssignEntity.getCreatedDate() );
        processTaskAssignDTO.setExtData( processTaskAssignEntity.getExtData() );
        processTaskAssignDTO.setModifiedDate( processTaskAssignEntity.getModifiedDate() );
        processTaskAssignDTO.setProductCode( processTaskAssignEntity.getProductCode() );
        processTaskAssignDTO.setStatus( processTaskAssignEntity.getStatus() );
        processTaskAssignDTO.setTaskNo( processTaskAssignEntity.getTaskNo() );

        return processTaskAssignDTO;
    }
}
