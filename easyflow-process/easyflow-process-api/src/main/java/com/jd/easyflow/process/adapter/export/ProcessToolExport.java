package com.jd.easyflow.process.adapter.export;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.RollbackNodeReq;
import com.jd.easyflow.process.adapter.export.dto.instance.RollbackNodeRes;

/**
 * @author liyuliang5
 */
public interface ProcessToolExport {

    ExportResponse<RollbackNodeRes> rollbackNode(ExportRequest<RollbackNodeReq> request);
    
    ExportResponse updateProcessInstance(ExportRequest<ProcessInstanceDTO> request);
    
    ExportResponse updateProcessNodeInstance(ExportRequest<ProcessNodeInstanceDTO> request);
    
    ExportResponse deleteProcessNodeInstance(ExportRequest<String> request);
    
}
