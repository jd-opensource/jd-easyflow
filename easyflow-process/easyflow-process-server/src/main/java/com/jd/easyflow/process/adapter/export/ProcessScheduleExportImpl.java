package com.jd.easyflow.process.adapter.export;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.process.adapter.export.converter.ProcessScheduleConverter;
import com.jd.easyflow.process.adapter.export.dto.schedule.ScheduleProcessReq;
import com.jd.easyflow.process.adapter.export.dto.schedule.ScheduleProcessRes;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessResVO;
import com.jd.easyflow.process.domain.service.ProcessScheduleDomainService;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessScheduleExportImpl implements ProcessScheduleExport {
    
    @Autowired
    private ProcessScheduleDomainService processScheduleDomainService;

    @Action(code = "easyflow-process-0401", name = "schedule")
    @Override
    public ExportResponse<ScheduleProcessRes> schedule(ExportRequest<ScheduleProcessReq> req) {
        ScheduleProcessReqVO reqVo = ProcessScheduleConverter.INSTANCE.convert(req.getData());
        ScheduleProcessResVO resVo = processScheduleDomainService.schedule(reqVo);
        ScheduleProcessRes res = ProcessScheduleConverter.INSTANCE.convert(resVo);
        return ExportResponse.build4Success(res);
    }

}
