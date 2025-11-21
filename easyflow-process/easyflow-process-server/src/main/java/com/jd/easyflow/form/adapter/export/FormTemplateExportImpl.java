package com.jd.easyflow.form.adapter.export;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.form.adapter.export.converter.FormTemplateConverter;
import com.jd.easyflow.form.adapter.export.converter.PagerConverter;
import com.jd.easyflow.form.adapter.export.dto.FormTemplateDTO;
import com.jd.easyflow.form.domain.model.entity.FormTemplateEntity;
import com.jd.easyflow.form.domain.repository.FormTemplateRepository;
import com.jd.easyflow.form.domain.service.FormTemplateDomainService;

/**
 * @author liyuliang5
 *
 */
public class FormTemplateExportImpl implements FormTemplateExport {

    @Autowired
    private FormTemplateDomainService formTemplateDomainService;
    @Autowired
    private FormTemplateRepository formTemplateRepository;

    @Override
    public ExportResponse<PagerResult> find(ExportRequest<PagerCondition> req) {
        com.jd.easyflow.common.dto.pager.PagerResult<FormTemplateEntity> pagerResult = formTemplateRepository
                .find(PagerConverter.INSTANCE.convert(req.getData()));
        return ExportResponse.build4Success(new PagerResult<>(pagerResult.getCount(),
                FormTemplateConverter.INSTANCE.convert(pagerResult.getList())));

    }

    @Override
    public ExportResponse<Void> add(ExportRequest<FormTemplateDTO> req) {
        FormTemplateEntity entity = FormTemplateConverter.INSTANCE.convert(req.getData());
        formTemplateRepository.add(entity);
        return ExportResponse.build4Success();
    }

    @Override
    public ExportResponse<Void> update(ExportRequest<FormTemplateDTO> req) {
        formTemplateDomainService.update(FormTemplateConverter.INSTANCE.convert(req.getData()));
        return ExportResponse.build4Success();
    }

    @Override
    public ExportResponse<FormTemplateDTO> get(ExportRequest<String> req) {
        String templateCode = req.getData();
        FormTemplateEntity entity = formTemplateRepository.get(templateCode);
        return ExportResponse.build4Success(FormTemplateConverter.INSTANCE.convert(entity));
    }

    public FormTemplateDomainService getFormTemplateDomainService() {
        return formTemplateDomainService;
    }

    public void setFormTemplateDomainService(FormTemplateDomainService formTemplateDomainService) {
        this.formTemplateDomainService = formTemplateDomainService;
    }

    public FormTemplateRepository getFormTemplateRepository() {
        return formTemplateRepository;
    }

    public void setFormTemplateRepository(FormTemplateRepository formTemplateRepository) {
        this.formTemplateRepository = formTemplateRepository;
    }
    
    

}
