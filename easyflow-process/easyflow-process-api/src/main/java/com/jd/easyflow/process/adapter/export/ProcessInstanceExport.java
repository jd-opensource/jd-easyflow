package com.jd.easyflow.process.adapter.export;

import java.util.List;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.process.adapter.export.dto.instance.*;

/**
 * @author liyuliang5
 *
 */
public interface ProcessInstanceExport {

    public static final String TYPE_PROCESS = "PROCESS";
    public static final String TYPE_NODE = "NODE";
    public static final String TYPE_EXECUTION = "EXECUTION";

    ExportResponse<CreateProcessInstanceRes> createProcessInstance(ExportRequest<CreateProcessInstanceReq> req);
    
    ExportResponse<String> lockProcessInstance(ExportRequest<LockProcessInstanceReq> req);

    ExportResponse<Boolean> unLockProcessInstance(ExportRequest<UnlockProcessInstanceReq> req);
    
    ExportResponse<Object> updateProcessInstance(ExportRequest<ProcessInstanceDTO> request);

    ExportResponse<CanCancelProcessInstanceRes> canCancel(ExportRequest<CanCancelProcessInstanceReq> request);

    ExportResponse<CancelProcessInstanceRes> cancel(ExportRequest<CancelProcessInstanceReq> request);
    
    /* process instance query */
    ExportResponse<PagerResult> pagerQueryProcessInstance(ExportRequest<PagerCondition> req);
    
    ExportResponse<ProcessInstanceDTO> getProcessInstance(ExportRequest<String> req);
    
    ExportResponse<ProcessInstanceDTO> queryProcessInstanceByProcessTypeAndBizNo(ExportRequest<QueryProcessInstanceReq> req);
    
    ExportResponse<ProcessInstanceDTO> queryActiveProcessInstanceByProcessTypeAndBizNo(ExportRequest<QueryProcessInstanceReq> req);
    
    ExportResponse<List<ProcessInstanceDTO>> queryInstanceByInstanceNos(ExportRequest<List<String>> request);
    
    ExportResponse<List<ProcessInstanceDTO>> queryProcessInstanceByParentInstanceNo(ExportRequest<String> request);

    /*process node instance query*/
    ExportResponse<List<ProcessNodeInstanceDTO>> queryProcessNodeInstanceByInstanceNo(ExportRequest<String> req);

    ExportResponse<ProcessNodeInstanceDTO> queryOpenNodeInstance(ExportRequest<QueryOpenNodeInstanceReq> request);

    ExportResponse<List<ProcessNodeInstanceDTO>> findNodeInstances(ExportRequest<QueryProcessNodeReqDTO> queryReq);

    ExportResponse<ProcessNodeInstanceDTO> queryNodeInstanceByNo(ExportRequest<String> request);

    ExportResponse<List<ProcessNodeInstanceDTO>> queryNodeInstanceByNos(ExportRequest<List<String>> request);

    /*process execution query*/
    ExportResponse<ProcessNodeExecutionDTO> queryNodeExecutionByNo(ExportRequest<String> request);


}
