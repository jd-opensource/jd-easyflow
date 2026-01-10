package com.jd.easyflow.processunit.adapter.export;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateRes;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteRes;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecutionDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecutionQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateRes;
import com.jd.easyflow.processunit.adapter.export.dto.ShardingInfoDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ShardingInfoQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ShutdownReq;
import com.jd.easyflow.processunit.adapter.export.dto.ShutdownRes;
import com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallReq;
import com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallRes;
import com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallReq;
import com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallRes;

/**
 * @author liyuliang5
 *
 */
public interface ProcessUnitExport {
    
    // tx

    public ExportResponse<SyncBeforeCallRes> syncBeforeCall(ExportRequest<SyncBeforeCallReq> req);
    
    public ExportResponse<SyncAfterCallRes> syncAfterCall(ExportRequest<SyncAfterCallReq> req);
    
    public ExportResponse<ProcessUnitCreateRes> create(ExportRequest<ProcessUnitCreateReq> req); 
    
    public ExportResponse<ProcessUnitExecuteRes> execute(ExportRequest<ProcessUnitExecuteReq> req);
    
    public ExportResponse<Void> batchExecute(ExportRequest<Map<String, String>> req);
    
    public ExportResponse<Object> updateProcessUnitInstanceByInstanceNoSelective(ExportRequest<ProcessUnitInstanceDTO> req);
    
    public ExportResponse<ProcessUnitUpdateRes> update(ExportRequest<ProcessUnitUpdateReq> req); 
    
    // read

    public ExportResponse<ProcessUnitInstanceDTO> getByProcessInstanceNo(ExportRequest<String> req);
    
    public ExportResponse<ProcessUnitInstanceDTO> getByBizNoAndProcessUnitCode(ExportRequest<ProcessUnitInstanceQueryReq> req);
    
    ExportResponse<List<ProcessUnitInstanceDTO>> queryByUnitCodeAndBizNoPrefix(
            ExportRequest<ProcessUnitInstanceQueryReq> req);
    
    public ExportResponse<PagerResult> pagerQueryProcessUnitInstance(ExportRequest<PagerCondition> req);

    public ExportResponse<PagerResult> pagerQueryProcessUnitExecution(ExportRequest<PagerCondition> req);

    public ExportResponse<ProcessUnitExecutionDTO> getExecutionByExecutionNo(ExportRequest<ProcessUnitExecutionQueryReq> req);
    
    public ExportResponse<List<ProcessUnitDTO>> queryProcessUnitList(ExportRequest<ProcessUnitQueryReq> req);

    public ExportResponse<ShardingInfoDTO> queryShardingInfo(ExportRequest<ShardingInfoQueryReq> req);
    
    public ExportResponse<ShutdownRes> shutdown(ExportRequest<ShutdownReq> req);

    
}
