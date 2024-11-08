package com.jd.easyflow.process.adapter.export;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.process.adapter.export.dto.definition.NodeDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.QueryNodeReq;
import com.jd.easyflow.process.adapter.export.dto.definition.QueryProcessDefinitionReq;

/**
 * @author liyuliang5
 *
 */
public interface ProcessDefinitionExport {
    
    public ExportResponse<ProcessDefinitionDTO> getLatestProcessDefinition(ExportRequest<String> req);

    public ExportResponse<ProcessDefinitionDTO> getVersionedProcessDefinition(ExportRequest<String> req);

    ExportResponse<ProcessDefinitionDTO> queryProcessDefinitionByVersion(ExportRequest<QueryProcessDefinitionReq> req);

    ExportResponse<ProcessDefinitionDTO> getProcessDefinition(ExportRequest<String> req);

    ExportResponse addProcessDefinition(ExportRequest<ProcessDefinitionDTO> processDefinitionReq);

    ExportResponse updateProcessDefinition(ExportRequest<ProcessDefinitionDTO> processDefinitionReq);

    ExportResponse<PagerResult<ProcessDefinitionDTO>> pageQueryProcessDefinition(ExportRequest<PagerCondition> pagerQueryReq);

    ExportResponse<Integer> getLatestProcessDefVersionByDefId(ExportRequest<String> request);

    ExportResponse reportProcessDef(ExportRequest<ProcessDefinitionDTO> processDefinitionReq);

    ExportResponse forceUpdateCurrentVersionProcessDef(ExportRequest<ProcessDefinitionDTO> processDefinitionReq);

    ExportResponse<ProcessDTO> getProcessProperties(ExportRequest<String> request);

    ExportResponse<NodeDTO> getNodeProperties(ExportRequest<QueryNodeReq> request);

    ExportResponse<ProcessDTO> getProcessAndNodeProperties(ExportRequest<String> request);
}
