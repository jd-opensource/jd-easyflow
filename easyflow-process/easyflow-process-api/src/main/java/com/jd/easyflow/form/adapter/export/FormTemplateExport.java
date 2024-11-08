package com.jd.easyflow.form.adapter.export;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.form.adapter.export.dto.FormTemplateDTO;

/**
 * @author liyuliang5
 *
 */
public interface FormTemplateExport {

    ExportResponse<PagerResult> find(ExportRequest<PagerCondition> pagerCondition);
    
    ExportResponse<Void> add(ExportRequest<FormTemplateDTO> formTemplate);
    
    ExportResponse<Void> update(ExportRequest<FormTemplateDTO> formTemplate);
        
    ExportResponse<FormTemplateDTO> get(ExportRequest<String> templateCode);
}
