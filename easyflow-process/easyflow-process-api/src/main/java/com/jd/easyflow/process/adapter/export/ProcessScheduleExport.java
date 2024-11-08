package com.jd.easyflow.process.adapter.export;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.process.adapter.export.dto.schedule.ScheduleProcessReq;

/**
 * @author liyuliang5
 *
 */
public interface ProcessScheduleExport {

    /**
     * @param req
     * @return
     */
    ExportResponse<Object> schedule(ExportRequest<ScheduleProcessReq> req);
}
