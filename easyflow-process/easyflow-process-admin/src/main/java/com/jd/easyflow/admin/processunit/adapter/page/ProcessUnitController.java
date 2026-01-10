package com.jd.easyflow.admin.processunit.adapter.page;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.context.request.WebRequest;

import com.jd.easyflow.admin.processunit.adapter.page.converter.PagerConverter;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.adapter.page.BasePageController;
import com.jd.easyflow.common.adapter.page.WebResponse;
import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.util.CommonErrorCode;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.processunit.adapter.export.ProcessUnitExport;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecutionDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecutionQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ShardingInfoDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ShardingInfoQueryReq;

/**
 * 
 * @author liyuliang5
 */
@Controller
@RequestMapping("easyflow/processUnit")
public class ProcessUnitController extends BasePageController {

    private static final Logger log = LoggerFactory.getLogger(ProcessUnitController.class);
    
    private static final String EXT_DATA_KEY_USING_SLAVE_DB = "_usingSlaveDb";
    
    private boolean pagerQueryUsingSlaveDb = true;

    private ProcessUnitExport processUnitExport;

    @RequestMapping("processUnitInstanceList")
    public String processUnitList(Model model) {
        ExportResponse<List<ProcessUnitDTO>> response = getProcessUnitExport()
                .queryProcessUnitList(new ExportRequest<ProcessUnitQueryReq>(new ProcessUnitQueryReq()));
        List<ProcessUnitDTO> processUnitList = ExportResponseUtil.unwrap(response);
        model.addAttribute("processUnitList", processUnitList);
        return "easyflow/processunit/processUnitInstanceList";
    }

    @RequestMapping("ajax/getProcessUnitInstanceListData")
    @ResponseBody
    public WebResponse<PagerResult> getInstanceData(NativeWebRequest request) {
        PagerCondition condition = getPagerCondition(request);
        FieldEntry resultListField = condition.getField("resultList");
        if (resultListField != null && resultListField.getValue() instanceof String) {
            resultListField.setValue(new String[] {(String) resultListField.getValue()});
        }
        return getInstanceData(condition);
    }

    protected WebResponse<PagerResult> getInstanceData(PagerCondition condition) {
        FieldEntry queryTypeField = condition.getField("queryType");
        FieldEntry createdDateStartField = condition.getField("createdDateStart");
        FieldEntry createdDateEndField = condition.getField("createdDateEnd");
        condition.putExtData(EXT_DATA_KEY_USING_SLAVE_DB, pagerQueryUsingSlaveDb);
        ExportResponse response = getProcessUnitExport()
                .pagerQueryProcessUnitInstance(new ExportRequest(PagerConverter.INSTANCE.convert(condition)));
        if (!ExportResponseCode.SUCCESS.getCode().equals(response.getResCode())) {
            return WebResponse.buildResponse(response.getResCode(), response.getResDesc());
        }
        com.jd.easyflow.common.adapter.export.dto.pager.PagerResult pagerResult = (com.jd.easyflow.common.adapter.export.dto.pager.PagerResult) ExportResponseUtil
                .unwrap(response);
        return WebResponse.buildResponse(CommonErrorCode.E0000000.getCode(), CommonErrorCode.E0000000.getDesc(),
                pagerResult);
    }

    @RequestMapping("processUnitInstanceDetail")
    public String instanceView(String instanceNo, String processUnitCode, String bizNo, Model model) {
        ProcessUnitInstanceQueryReq req = new ProcessUnitInstanceQueryReq();
        req.setProcessUnitCode(processUnitCode);
        req.setBizNo(bizNo);
        ExportResponse<ProcessUnitInstanceDTO> response = getProcessUnitExport()
                .getByBizNoAndProcessUnitCode(new ExportRequest<ProcessUnitInstanceQueryReq>(req));
        ProcessUnitInstanceDTO instance = ExportResponseUtil.unwrap(response);
        model.addAttribute("detail", instance);

        
        ShardingInfoQueryReq query = new ShardingInfoQueryReq();
        query.setUnitCode(processUnitCode);
        query.setBizNo(bizNo);
        ExportResponse<ShardingInfoDTO> shardResponse = getProcessUnitExport().queryShardingInfo(new ExportRequest<ShardingInfoQueryReq>(query));
        ShardingInfoDTO shardInfo = ExportResponseUtil.unwrap(shardResponse);
        model.addAttribute("shardInfo", shardInfo);

        return "easyflow/processunit/processUnitInstanceDetail";
    }

    @RequestMapping("processUnitExecutionList")
    public String processUnitExecutionList(Model model) {
        ExportResponse<List<ProcessUnitDTO>> response = getProcessUnitExport()
                .queryProcessUnitList(new ExportRequest<ProcessUnitQueryReq>(new ProcessUnitQueryReq()));
        List<ProcessUnitDTO> processUnitList = ExportResponseUtil.unwrap(response);
        model.addAttribute("processUnitList", processUnitList);
        return "easyflow/processunit/processUnitExecutionList";
    }

    @RequestMapping("ajax/getProcessUnitExecutionListData")
    @ResponseBody
    public WebResponse<PagerResult> getExecutionData(NativeWebRequest request) {
        PagerCondition condition = getPagerCondition(request);
        FieldEntry resultListField = condition.getField("resultList");
        if (resultListField != null && resultListField.getValue() instanceof String) {
            resultListField.setValue(new String[] {(String) resultListField.getValue()});
        }
        return getExecutionData(condition);
    }

    protected WebResponse<PagerResult> getExecutionData(PagerCondition condition) {
        condition.putExtData(EXT_DATA_KEY_USING_SLAVE_DB, pagerQueryUsingSlaveDb);
        ExportResponse response = getProcessUnitExport()
                .pagerQueryProcessUnitExecution(new ExportRequest(PagerConverter.INSTANCE.convert(condition)));
        if (!ExportResponseCode.SUCCESS.getCode().equals(response.getResCode())) {
            return WebResponse.buildResponse(response.getResCode(), response.getResDesc());
        }
        com.jd.easyflow.common.adapter.export.dto.pager.PagerResult pagerResult = (com.jd.easyflow.common.adapter.export.dto.pager.PagerResult) ExportResponseUtil
                .unwrap(response);        
        return WebResponse.buildResponse(CommonErrorCode.E0000000.getCode(), CommonErrorCode.E0000000.getDesc(),
                pagerResult);
    }

    @RequestMapping("processUnitExecutionDetail")
    public String executionView(String executionNo, String processUnitCode, String bizNo, Model model) {
        ProcessUnitExecutionQueryReq req = new ProcessUnitExecutionQueryReq();
        req.setProcessUnitCode(processUnitCode);
        req.setBizNo(bizNo);
        req.setExecutionNo(executionNo);
        ExportResponse<ProcessUnitExecutionDTO> response = getProcessUnitExport()
                .getExecutionByExecutionNo(new ExportRequest<ProcessUnitExecutionQueryReq>(req));
        ProcessUnitExecutionDTO execution = ExportResponseUtil.unwrap(response);
        model.addAttribute("detail", execution);
        return "easyflow/processunit/processUnitExecutionDetail";
    }

    protected ProcessUnitExport getProcessUnitExport() {
        if (processUnitExport == null) {
            processUnitExport = ObjectFactorys.getDefault().getObject(ProcessUnitExport.class);
        }
        return processUnitExport;
    }

    public boolean isPagerQueryUsingSlaveDb() {
        return pagerQueryUsingSlaveDb;
    }

    public void setPagerQueryUsingSlaveDb(boolean pagerQueryUsingSlaveDb) {
        this.pagerQueryUsingSlaveDb = pagerQueryUsingSlaveDb;
    }

    public void setProcessUnitExport(ProcessUnitExport processUnitExport) {
        this.processUnitExport = processUnitExport;
    }
    
    

}
