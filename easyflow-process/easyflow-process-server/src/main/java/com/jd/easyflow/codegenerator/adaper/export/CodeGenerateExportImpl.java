package com.jd.easyflow.codegenerator.adaper.export;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.codegenerator.adapter.export.CodeGenerateExport;
import com.jd.easyflow.codegenerator.adapter.export.dto.BatchGenerateParam;
import com.jd.easyflow.codegenerator.adapter.export.dto.BatchGenerateResult;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateParam;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateResult;
import com.jd.easyflow.codegenerator.domain.model.vo.CodeGenerateReq;
import com.jd.easyflow.codegenerator.domain.service.CodeGenerateDomainService;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;

/**
 * @author liyuliang5
 *
 */
public class CodeGenerateExportImpl implements CodeGenerateExport {
    
    @Autowired
    private CodeGenerateDomainService codeGenerateDomainService;

    @Override
    public ExportResponse<GenerateResult> generateUniqueCode(ExportRequest<GenerateParam> request) {
        GenerateParam param = request.getData();
        CodeGenerateReq codeGenerateReq = new CodeGenerateReq(param.getTypeId(), param.getCodePrefix());
        String code = codeGenerateDomainService.next(codeGenerateReq);
        GenerateResult result = new GenerateResult();
        result.setCode(code);
        return ExportResponse.build4Success(result);
    }

    @Override
    public ExportResponse<BatchGenerateResult> batchGenerateUniqueCode(ExportRequest<BatchGenerateParam> request) {
        BatchGenerateParam param = request.getData();
        CodeGenerateReq codeGenerateReq = new CodeGenerateReq(param.getTypeId(), param.getCodePrefix(),
                param.getBatchSize());
        String[] codes = codeGenerateDomainService.nextBatch(codeGenerateReq);
        BatchGenerateResult result = new BatchGenerateResult();
        result.setCodes(codes);
        return ExportResponse.build4Success(result);
    }
}
