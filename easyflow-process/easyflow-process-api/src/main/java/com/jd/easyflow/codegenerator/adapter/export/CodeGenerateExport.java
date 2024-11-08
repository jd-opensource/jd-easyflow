package com.jd.easyflow.codegenerator.adapter.export;

import com.jd.easyflow.codegenerator.adapter.export.dto.BatchGenerateParam;
import com.jd.easyflow.codegenerator.adapter.export.dto.BatchGenerateResult;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateParam;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateResult;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;

/**
 * @author liyuliang5
 *
 */
public interface CodeGenerateExport {

    public ExportResponse<GenerateResult> generateUniqueCode(ExportRequest<GenerateParam> request);
    
    public ExportResponse<BatchGenerateResult> batchGenerateUniqueCode(ExportRequest<BatchGenerateParam> request);
}
