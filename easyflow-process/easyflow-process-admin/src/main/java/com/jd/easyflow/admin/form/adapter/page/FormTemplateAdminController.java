package com.jd.easyflow.admin.form.adapter.page;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.jd.easyflow.admin.form.adapter.page.converter.PagerConverter;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.adapter.page.BasePageController;
import com.jd.easyflow.common.dto.DataResponse;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.form.adapter.export.FormTemplateExport;
import com.jd.easyflow.form.adapter.export.dto.FormTemplateDTO;
import com.jd.easyflow.objects.factory.ObjectFactorys;

/**
 * 
 * @author liyuliang5
 *
 */
@Controller
public class FormTemplateAdminController extends BasePageController {

    private FormTemplateExport formTemplateExport;

    @RequestMapping("easyflow/formTemplate/formTemplateList")
    public String templateList(Model model) {
        return "easyflow/formtemplate/formTemplateList";
    }

    @RequestMapping("easyflow/formTemplate/ajax/getListData")
    @ResponseBody
    public DataResponse<com.jd.easyflow.common.adapter.export.dto.pager.PagerResult> getData() {
        PagerCondition condition = getPagerCondition();
        ExportResponse<com.jd.easyflow.common.adapter.export.dto.pager.PagerResult> response = getFormTemplateExport()
                .find(new ExportRequest(PagerConverter.INSTANCE.convert(condition)));
        response.getData().getList().forEach(o -> {
            ((FormTemplateDTO) o).setConfig(null);
        });
        return new DataResponse<com.jd.easyflow.common.adapter.export.dto.pager.PagerResult>(
                ExportResponseUtil.unwrap(response));
    }

    @RequestMapping("easyflow/formTemplate/formTemplateDetail")
    public String detailPage(String templateCode, Model model) {
        ExportResponse<FormTemplateDTO> response = getFormTemplateExport().get(new ExportRequest(templateCode));
        FormTemplateDTO formTemplate = ExportResponseUtil.unwrap(response);
        model.addAttribute("detail", formTemplate);
        return "easyflow/formtemplate/formTemplateDetail";
    }

    @RequestMapping("easyflow/formTemplate/formTemplateAdd")
    public String addPage(Model model) {
        return "easyflow/formtemplate/formTemplateAdd";
    }

    @RequestMapping("easyflow/formTemplate/formTemplateEdit")
    public String editPage(String templateCode, Model model) {
        ExportResponse<FormTemplateDTO> response = getFormTemplateExport().get(new ExportRequest(templateCode));
        model.addAttribute("detail", ExportResponseUtil.unwrap(response));
        return "easyflow/formtemplate/formTemplateEdit";
    }

    @RequestMapping("easyflow/formTemplate/ajax/add")
    @ResponseBody
    public DataResponse<Void> add(FormTemplateDTO formTemplate) {
        getFormTemplateExport().add(new ExportRequest(formTemplate));
        return new DataResponse<>();
    }

    @RequestMapping("easyflow/formTemplate/ajax/edit")
    @ResponseBody
    public DataResponse<Void> edit(FormTemplateDTO formTemplate) {
        getFormTemplateExport().update(new ExportRequest(formTemplate));
        return new DataResponse<>();
    }

    @RequestMapping("easyflow/formTemplate/ajax/getTemplate")
    @ResponseBody
    public DataResponse<Map<String, Object>> getTemplate(String templateCode) {
        ExportResponse<FormTemplateDTO> response = getFormTemplateExport().get(new ExportRequest(templateCode));
        Map<String, Object> result = new HashMap<>();
        String config = null == ExportResponseUtil.unwrap(response) ? null : ExportResponseUtil.unwrap(response).getConfig();
        result.put("config", config);
        return new DataResponse<>(result);
    }

    public FormTemplateExport getFormTemplateExport() {
        if (formTemplateExport == null) {
            formTemplateExport = ObjectFactorys.getDefault().getObject(FormTemplateExport.class);
        }
        return formTemplateExport;
    }

}
