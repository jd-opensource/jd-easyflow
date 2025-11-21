package com.jd.easyflow.process.domain.service;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.process.domain.model.vo.ScheduleProcessReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessResVO;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessScheduleDomainService {

    @Autowired
    private ProcessScheduleInvoker invoker;

    public ScheduleProcessResVO schedule(ScheduleProcessReqVO vo) {
        return invoker.invoke(vo);
    }

    public ProcessScheduleInvoker getInvoker() {
        return invoker;
    }

    public void setInvoker(ProcessScheduleInvoker invoker) {
        this.invoker = invoker;
    }
    
    

}
