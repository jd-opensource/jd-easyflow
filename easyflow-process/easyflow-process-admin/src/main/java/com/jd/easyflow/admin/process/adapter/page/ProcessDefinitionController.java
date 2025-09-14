package com.jd.easyflow.admin.process.adapter.page;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.jd.easyflow.admin.process.adapter.page.converter.PagerConverter;
import com.jd.easyflow.admin.process.adapter.page.converter.ProcessDefinitionConverter;
import com.jd.easyflow.admin.process.adapter.page.dto.ProcessDefDTO;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.adapter.page.BasePageController;
import com.jd.easyflow.common.adapter.page.WebResponse;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.common.util.CommonErrorCode;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessDefinitionExport;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.QueryProcessDefinitionReq;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
@Controller
@RequestMapping("easyflow/processDefinition")
public class ProcessDefinitionController extends BasePageController {

    private final static int INIT_VERSION = 0;

    private final static String MANUAL = "MANUAL";

    private ProcessDefinitionExport processDefinitionExport;

    public ProcessDefinitionController() {
    }


    @RequestMapping("list")
    public String processDefList() {
        return "easyflow/process/processdefinition/processDefinitionList";
    }
    
    @RequestMapping("ajax/getListData")
    @ResponseBody
    public WebResponse<PagerResult> getData() {
        PagerCondition condition = getPagerCondition();
        return getData(condition);
    }

    protected WebResponse<PagerResult> getData(PagerCondition condition) {
        ExportResponse<com.jd.easyflow.common.adapter.export.dto.pager.PagerResult<ProcessDefinitionDTO>> response = getProcessDefinitionExport()
                .pageQueryProcessDefinition(new ExportRequest(PagerConverter.INSTANCE.convert(condition)));
        com.jd.easyflow.common.adapter.export.dto.pager.PagerResult<ProcessDefinitionDTO> result = ExportResponseUtil
                .unwrap(response);
        List<ProcessDefDTO> list = new ArrayList<>();
        for (ProcessDefinitionDTO definition : result.getList()) {
            list.add(ProcessDefinitionConverter.INSTANCE.adapterConvert(definition));
        }
        return WebResponse.buildResponse(CommonErrorCode.E0000000.getCode(), CommonErrorCode.E0000000.getDesc(),
                new PagerResult<>(result.getCount(), list));
    }


    @RequestMapping("ajax/processSelectFrame")
    public String processSelectFrame(Model model, String selectId) {
        model.addAttribute("selectId", selectId);
        return "easyflow/process/processdefinition/processSelectFrame";
    }

    @RequestMapping("add")
    public String addPage() {
        return "easyflow/process/processdefinition/processDefinitionAdd";
    }


    @RequestMapping("edit")
    public String editPage(String defId, Integer defVersion, Model model) {
        QueryProcessDefinitionReq req = new QueryProcessDefinitionReq();
        req.setDefId(defId);
        req.setDefVersion(defVersion);
        ExportResponse<ProcessDefinitionDTO> response = getProcessDefinitionExport()
                .queryProcessDefinitionByVersion(new ExportRequest<QueryProcessDefinitionReq>(req));
        if (!CommonErrorCode.E0000000.getCode().equals(response.getResCode())) {
            throw new UserException(response.getResCode(), response.getResDesc());
        }
        ProcessDefDTO processDef = ProcessDefinitionConverter.INSTANCE.adapterConvert(response.getData());
        model.addAttribute("definition", processDef);
        model.addAttribute("defData", JSON.toJSONString(processDef));
        return "easyflow/process/processdefinition/processDefinitionEdit";
    }

    @RequestMapping("detail")
    public String processDefView(String defId, Integer defVersion, boolean latest, String fullDefinitionId,
            Model model) {
        ExportResponse<ProcessDefinitionDTO> exportResponse;
        if (latest) {
            exportResponse = getProcessDefinitionExport().getLatestProcessDefinition(new ExportRequest(defId));
        } else {
            if (fullDefinitionId != null && ! fullDefinitionId.isEmpty()) {
                // Process instance page definition link to here, empty id represent empty version.
                exportResponse = getProcessDefinitionExport()
                        .getProcessDefinition(new ExportRequest<String>(fullDefinitionId));
            } else {
                QueryProcessDefinitionReq req = new QueryProcessDefinitionReq();
                req.setDefId(defId);
                req.setDefVersion(defVersion);
                exportResponse = getProcessDefinitionExport().queryProcessDefinitionByVersion(new ExportRequest(req));
            }
        }
        if (!CommonErrorCode.E0000000.getCode().equals(exportResponse.getResCode())) {
            throw new UserException(exportResponse.getResCode(), exportResponse.getResDesc());
        }
        ProcessDefDTO processDef = ProcessDefinitionConverter.INSTANCE.adapterConvert(exportResponse.getData());
        model.addAttribute("defData", JSON.toJSONString(processDef));
        return "easyflow/process/processdefinition/processDefinitionDetail";
    }

    @RequestMapping("ajax/add")
    @ResponseBody
    public WebResponse add(ProcessDefDTO processDef) {
        ProcessDefinitionDTO processDefinitionReq = ProcessDefinitionConverter.INSTANCE.adapterConvert(processDef);
        processDefinitionReq.setDefVersion(INIT_VERSION);
        processDefinitionReq.setDefSource(MANUAL);
        ExportResponse exportResponse = getProcessDefinitionExport()
                .addProcessDefinition(new ExportRequest(processDefinitionReq));
        return WebResponse.buildResponse(exportResponse.getResCode(), exportResponse.getResDesc());
    }

    @RequestMapping("ajax/edit")
    @ResponseBody
    public WebResponse<Void> edit(ProcessDefDTO processDef) {
        ProcessDefinitionDTO processDefinitionReq = ProcessDefinitionConverter.INSTANCE.adapterConvert(processDef);
        processDefinitionReq.setDefSource(MANUAL);
        ExportResponse response = getProcessDefinitionExport()
                .updateProcessDefinition(new ExportRequest(processDefinitionReq));
        return WebResponse.buildResponse(response.getResCode(), response.getResDesc());
    }

    @RequestMapping("ajax/forceUpdate")
    @ResponseBody
    public WebResponse<Void> forceUpdate(ProcessDefDTO processDef) {
        ProcessDefinitionDTO processDefinitionReq = ProcessDefinitionConverter.INSTANCE.adapterConvert(processDef);
        processDefinitionReq.setDefSource(MANUAL);
        ExportResponse response = getProcessDefinitionExport()
                .forceUpdateCurrentVersionProcessDef(new ExportRequest(processDefinitionReq));
        return WebResponse.buildResponse(response.getResCode(), response.getResDesc());
    }

    protected ProcessDefinitionExport getProcessDefinitionExport() {
        if (processDefinitionExport == null) {
            processDefinitionExport = ObjectFactorys.getDefault().getObject(ProcessDefinitionExport.class);
        }
        return processDefinitionExport;
    }



}
