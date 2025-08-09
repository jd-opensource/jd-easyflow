package com.jd.easyflow.process.adapter.export;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdRes;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnRes;

/**
 * @author liyuliang5
 *
 */
public interface ProcessTransactionExport {
    
    ExportResponse<String> nextObjectId(ExportRequest<String> rquest);
    
    ExportResponse<BatchObjectIdRes> batchNextObjectId(ExportRequest<BatchObjectIdReq> request);

    ExportResponse<TxnRes> doTransaction(ExportRequest<TxnReq> request);
}
