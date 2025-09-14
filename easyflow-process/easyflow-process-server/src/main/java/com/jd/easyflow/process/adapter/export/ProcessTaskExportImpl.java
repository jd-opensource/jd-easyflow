package com.jd.easyflow.process.adapter.export;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.process.adapter.export.converter.PagerConverter;
import com.jd.easyflow.process.adapter.export.converter.ProcessTaskConverter;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskRes;
import com.jd.easyflow.process.adapter.export.dto.task.ExecuteTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.ExecuteTaskRes;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.QueryTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperationsReq;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperationsRes;
import com.jd.easyflow.process.adapter.export.dto.task.WithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.WithdrawTaskRes;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskAssignEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.model.vo.ExecuteProcessTaskReqVO;
import com.jd.easyflow.process.domain.model.vo.QueryTaskReqVO;
import com.jd.easyflow.process.domain.repository.ProcessTaskRepository;
import com.jd.easyflow.process.domain.service.ProcessTaskDomainService;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTaskExportImpl implements ProcessTaskExport {

    @Autowired
    private ProcessTaskDomainService processTaskDomainService;
    @Autowired
    private ProcessTaskRepository processTaskRepository;

    @Action(code = "easyflow-process-executeTask", name = "executeTask")
    @Override
    public ExportResponse<ExecuteTaskRes> executeTask(ExportRequest<ExecuteTaskReq> req) {
        ExecuteProcessTaskReqVO vo = ProcessTaskConverter.INSTANCE.convert(req.getData());
        processTaskDomainService.executeTask(vo);
        ExecuteTaskRes res = new ExecuteTaskRes();
        return ExportResponse.build4Success(res);
    }

    @Action(code = "easyflow-process-pagerQueryTask", name = "pagerQueryTask")
    @Override
    public ExportResponse<PagerResult> pagerQueryTask(ExportRequest<PagerCondition> req) {
        com.jd.easyflow.common.dto.pager.PagerCondition condition = PagerConverter.INSTANCE.convert(req.getData());
        com.jd.easyflow.common.dto.pager.PagerResult result = getTransactionTemplate().execute(status -> {
            return processTaskDomainService.pagerQueryTask(condition);
        });

        return ExportResponse.build4Success(PagerConverter.INSTANCE.convert(result));
    }

    @Action(code = "easyflow-process-getTask", name = "getTask")
    @Override
    public ExportResponse<ProcessTaskDTO> getTask(ExportRequest<String> req) {
        ProcessTaskDTO processTask = getTransactionTemplate().execute(status -> {
            return processTaskDomainService.getTask(req.getData());
        });
        return ExportResponse.build4Success(processTask);
    }

    @Action(code = "easyflow-process-queryTask", name = "queryTask")
    @Override
    public ExportResponse<List<ProcessTaskDTO>> queryTask(ExportRequest<QueryTaskReq> req) {
        QueryTaskReqVO query = ProcessTaskConverter.INSTANCE.convert(req.getData());
        List<ProcessTaskEntity> list = getTransactionTemplate().execute(status -> {
            return processTaskDomainService.queryTask(query);
        });
        return ExportResponse.build4Success(ProcessTaskConverter.INSTANCE.convertEntityList(list));
    }

    @Action(code = "easyflow-process-canWithdraw", name = "canWithdraw")
    @Override
    public ExportResponse<CanWithdrawTaskRes> canWithdraw(ExportRequest<CanWithdrawTaskReq> req) {
        CanWithdrawTaskRes res = getTransactionTemplate().execute(status -> {
            return processTaskDomainService.canWithdrawTask(req.getData().getTaskNo(), req.getData().getUser());
        });
        return ExportResponse.build4Success(res);
    }

    @Action(code = "easyflow-process-withDraw", name = "withDraw")
    @Override
    public ExportResponse<WithdrawTaskRes> withDraw(ExportRequest<WithdrawTaskReq> req) {
        processTaskDomainService.withdrawTask(req.getData().getTaskNo(), req.getData().getUser(),
                req.getData().getWithdrawInstancePolicy(), req.getData().getInstanceBizStatus(),
                req.getData().getInstanceBizData());
        return ExportResponse.build4Success(new WithdrawTaskRes());
    }

    @Action(code = "easyflow-process-doExecuteOperations", name = "doExecuteOperations")
    @Override
    public ExportResponse<TaskOperationsRes> doExecuteOperations(ExportRequest<TaskOperationsReq> req) {
        throw new UnsupportedOperationException();
    }

    @Action(code = "easyflow-process-findTaskAssignListByTaskNo", name = "findTaskAssignListByTaskNo")
    @Override
    public ExportResponse<List<ProcessTaskAssignDTO>> findTaskAssignListByTaskNo(ExportRequest<String> req) {
        List<ProcessTaskAssignEntity> assignList = getTransactionTemplate().execute(status -> {
            return processTaskRepository.findTaskAssignListByTaskNo(req.getData());
        });
        return ExportResponse.build4Success(ProcessTaskConverter.INSTANCE.convertAssignList(assignList));
    }
    
    private TransactionTemplate getTransactionTemplate() {
        return processTaskDomainService.getTransactionTemplate();
    }

    public ProcessTaskDomainService getProcessTaskDomainService() {
        return processTaskDomainService;
    }

    public void setProcessTaskDomainService(ProcessTaskDomainService processTaskDomainService) {
        this.processTaskDomainService = processTaskDomainService;
    }

    public ProcessTaskRepository getProcessTaskRepository() {
        return processTaskRepository;
    }

    public void setProcessTaskRepository(ProcessTaskRepository processTaskRepository) {
        this.processTaskRepository = processTaskRepository;
    }
    
    

}
