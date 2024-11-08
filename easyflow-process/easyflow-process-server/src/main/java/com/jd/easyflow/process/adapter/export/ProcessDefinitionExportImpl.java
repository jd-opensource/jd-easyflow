package com.jd.easyflow.process.adapter.export;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.process.adapter.export.converter.PagerConverter;
import com.jd.easyflow.process.adapter.export.converter.ProcessDefinitionConverter;
import com.jd.easyflow.process.adapter.export.dto.definition.NodeDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.QueryNodeReq;
import com.jd.easyflow.process.adapter.export.dto.definition.QueryProcessDefinitionReq;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.vo.ProcessDefinitionForListVO;
import com.jd.easyflow.process.domain.service.ProcessDefinitionDomainService;
import com.jd.easyflow.spring.MessageUtil;

/**
 *
 * @author liyuliang5
 */
public class ProcessDefinitionExportImpl implements ProcessDefinitionExport {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessDefinitionExportImpl.class);


    @Autowired
    private ProcessDefinitionDomainService processDefinitionDomainService;

    /**
     *
     * @param req
     */
    @Action(code = "easyflow-process-0101", name = "getLatestProcessDefinition")
    @Override
    public ExportResponse<ProcessDefinitionDTO> getLatestProcessDefinition(ExportRequest<String> req) {
        String definitionId = req.getData();
        ProcessDefinitionEntity entity = processDefinitionDomainService.getLatestProcessDefinition(definitionId);
        ProcessDefinitionDTO dto = ProcessDefinitionConverter.INSTANCE.convert(entity);
        return ExportResponse.build4Success(dto);
    }

    @Override
    @Action(code = "easyflow-process-0102", name = "getVersionedProcessDefinition")
    public ExportResponse<ProcessDefinitionDTO> getVersionedProcessDefinition(ExportRequest<String> req) {
        String definitionId = req.getData();
        ProcessDefinitionEntity entity = processDefinitionDomainService.getVersionedProcessDefinition(definitionId);
        ProcessDefinitionDTO dto = ProcessDefinitionConverter.INSTANCE.convert(entity);
        return ExportResponse.build4Success(dto);
    }

    /**
     *
     * @param req Flow ID(May contains version). format: {FLOW-|FSM-}{Flow ID}--V_{Version}
     */
    @Action(code = "easyflow-process-0103", name = "getProcessDefinition")
    @Override
    public ExportResponse<ProcessDefinitionDTO> getProcessDefinition(ExportRequest<String> req) {
        String definitionId = req.getData();
        ProcessDefinitionEntity processDefinitionVO = processDefinitionDomainService.getProcessDefinition(definitionId);
        ProcessDefinitionDTO dto = ProcessDefinitionConverter.INSTANCE.convert(processDefinitionVO);
        return ExportResponse.build4Success(dto);
    }

    @Action(code = "easyflow-process-0104", name = "addProcessDefinition")
    @Override
    public ExportResponse addProcessDefinition(ExportRequest<ProcessDefinitionDTO> processDefinitionReq) {
        ProcessDefinitionDTO definitionDTO = processDefinitionReq.getData();
        boolean exist = processDefinitionDomainService.existProcessDefinition(definitionDTO.getDefId());
        if (exist) {
            return ExportResponse.build4Failed(ExportResponseCode.FAIL.getCode(), MessageUtil.getMessage("easyflow.process.server.tip.processDefinitionExists"));
        }
        ProcessDefinitionEntity processDefinitionVO = ProcessDefinitionConverter.INSTANCE.convert(definitionDTO);
        processDefinitionDomainService.addProcessDefinition(processDefinitionVO);
        return ExportResponse.build4Success();
    }

    @Action(code = "easyflow-process-0105", name = "updateProcessDefinition")
    @Override
    public ExportResponse updateProcessDefinition(ExportRequest<ProcessDefinitionDTO> processDefinitionReq) {
        ProcessDefinitionDTO definitionDTO = processDefinitionReq.getData();
        boolean exist = processDefinitionDomainService.existProcessDefinition(definitionDTO.getDefId());
        if (!exist) {
            return ExportResponse.build4Failed(ExportResponseCode.DATA_EMPTY.getCode(), MessageUtil.getMessage("easyflow.process.server.tip.processDefinitionNotExists"));
        }
        ProcessDefinitionEntity processDefinitionEntity = ProcessDefinitionConverter.INSTANCE.convert(definitionDTO);
        processDefinitionDomainService.updateProcessDefinition(processDefinitionEntity);
        return ExportResponse.build4Success();
    }


    @Action(code = "easyflow-process-0106", name = "pageQueryProcessDefinition")
    @Override
    public ExportResponse<PagerResult<ProcessDefinitionDTO>> pageQueryProcessDefinition(ExportRequest<PagerCondition> pagerQueryReq) {
        com.jd.easyflow.common.dto.pager.PagerCondition condition = PagerConverter.INSTANCE.convert(pagerQueryReq.getData());
        com.jd.easyflow.common.dto.pager.PagerResult<ProcessDefinitionForListVO> pagerResult = processDefinitionDomainService.pageQueryProcessDefinition(condition);
        PagerResult<ProcessDefinitionDTO> result = ProcessDefinitionConverter.INSTANCE.convert(pagerResult);
        return ExportResponse.build4Success(result);
    }


    @Action(code = "easyflow-process-0107", name = "queryProcessDefinitionByVersion")
    @Override
    public ExportResponse<ProcessDefinitionDTO> queryProcessDefinitionByVersion(
            ExportRequest<QueryProcessDefinitionReq> req) {
        QueryProcessDefinitionReq query = req.getData();
        ExportResponse response;
        ProcessDefinitionEntity processDefinitionEntity = processDefinitionDomainService.findProcessDefinitionByIdAndVersion(query.getDefId(), query.getDefVersion());
        return ExportResponse.build4Success(ProcessDefinitionConverter.INSTANCE.convert(processDefinitionEntity));
    }


    @Action(code = "easyflow-process-0108", name = "getLatestProcessDefVersionByDefId")
    @Override
    public ExportResponse<Integer> getLatestProcessDefVersionByDefId(ExportRequest<String> request) {
        String definitionId = request.getData();
        if (StringUtils.isBlank(definitionId)){
            return ExportResponse.build4Failed(ExportResponseCode.FIELD_EMPTY);
        }
        Integer latestDefVersion = processDefinitionDomainService.getLatestProcessDefVersionByDefId(definitionId);
        if (latestDefVersion != null && latestDefVersion == -1) {
            ExportResponse response = new ExportResponse<>();
            response.setResCode(ExportResponseCode.DATA_EMPTY.getCode());
            response.setResDesc(MessageUtil.getMessage("easyflow.process.server.tip.processDefinitionNotExists"));
            return response;
        }
        return ExportResponse.build4Success(latestDefVersion);
    }


    @Action(code = "easyflow-process-0109", name = "reportProcessDef")
    @Override
    public ExportResponse reportProcessDef(ExportRequest<ProcessDefinitionDTO> processDefinitionReq) {
        ProcessDefinitionDTO processDefDTO = processDefinitionReq.getData();
        ProcessDefinitionEntity processDefinitionEntity = ProcessDefinitionConverter.INSTANCE.convert(processDefDTO);
        try {
            processDefinitionDomainService.reportProcessDef(processDefinitionEntity);
            return ExportResponse.build4Success();
        }catch (Exception e){
            log.error("Process definition report error," + processDefDTO.getDefId() + "," + e.getMessage(), e);
        }
        return ExportResponse.build4Failed(ExportResponseCode.FAIL);
    }


    @Action(code = "easyflow-process-0110", name = "forceUpdateCurrentVersionProcessDef")
    @Override
    public ExportResponse forceUpdateCurrentVersionProcessDef(ExportRequest<ProcessDefinitionDTO> processDefinitionReq) {
        ProcessDefinitionDTO definitionDTO = processDefinitionReq.getData();
        boolean exist = processDefinitionDomainService.existProcessDefinition(definitionDTO.getDefId());
        if (!exist) {
            return ExportResponse.build4Failed(ExportResponseCode.DATA_EMPTY.getCode(), MessageUtil.getMessage("easyflow.process.server.tip.processDefinitionNotExists"));
        }
        ProcessDefinitionEntity processDefinitionEntity = ProcessDefinitionConverter.INSTANCE.convert(definitionDTO);
        processDefinitionDomainService.forceUpdateProcessDefinition(processDefinitionEntity);
        return ExportResponse.build4Success();
    }

    @Action(code = "easyflow-process-0111", name = "getProcessProperties")
    @Override
    public ExportResponse<ProcessDTO> getProcessProperties(ExportRequest<String> request) {
        ProcessDTO processDto = processDefinitionDomainService.getProcessProperties(request.getData());
        return ExportResponse.build4Success(processDto);
    }

    @Action(code = "easyflow-process-0112", name = "getNodeProperties")
    @Override
    public ExportResponse<NodeDTO> getNodeProperties(ExportRequest<QueryNodeReq> request) {
        NodeDTO nodeDto = processDefinitionDomainService.getNodeProperties(request.getData());
        return ExportResponse.build4Success(nodeDto);
    }

    @Action(code = "easyflow-process-0113", name = "getProcessAndNodeProperties")
    @Override
    public ExportResponse<ProcessDTO> getProcessAndNodeProperties(ExportRequest<String> request) {
        ProcessDTO processDto = processDefinitionDomainService.getProcessAndNodeProperties(request.getData());
        return ExportResponse.build4Success(processDto);
    }

}
