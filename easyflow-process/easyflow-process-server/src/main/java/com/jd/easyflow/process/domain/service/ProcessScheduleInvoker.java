package com.jd.easyflow.process.domain.service;

import com.jd.easyflow.process.domain.model.vo.ScheduleProcessReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessResVO;

/**
 * @author liyuliang5
 *
 */
public interface ProcessScheduleInvoker {

    ScheduleProcessResVO invoke(ScheduleProcessReqVO vo);
}
