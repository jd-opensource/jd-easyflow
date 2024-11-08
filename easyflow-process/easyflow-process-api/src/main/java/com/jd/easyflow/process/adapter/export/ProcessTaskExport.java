package com.jd.easyflow.process.adapter.export;

import java.util.List;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskRes;
import com.jd.easyflow.process.adapter.export.dto.task.ExecuteTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.QueryTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperationsReq;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperationsRes;
import com.jd.easyflow.process.adapter.export.dto.task.WithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.WithdrawTaskRes;

/**

 * @author liyuliang5
 *
 */
public interface ProcessTaskExport {
    
    ExportResponse<List<ProcessTaskDTO>> queryTask(ExportRequest<QueryTaskReq> req);
    
    ExportResponse<Object> executeTask(ExportRequest<ExecuteTaskReq> req);
    
    ExportResponse<PagerResult> pagerQueryTask(ExportRequest<PagerCondition> req);

    ExportResponse<ProcessTaskDTO> getTask(ExportRequest<String> req);

    ExportResponse<CanWithdrawTaskRes> canWithdraw(ExportRequest<CanWithdrawTaskReq> req);

    ExportResponse<WithdrawTaskRes> withDraw(ExportRequest<WithdrawTaskReq> req);

    ExportResponse<TaskOperationsRes> doExecuteOperations(ExportRequest<TaskOperationsReq> req);  

    ExportResponse<List<ProcessTaskAssignDTO>> findTaskAssignListByTaskNo(ExportRequest<String> req);
    

}
