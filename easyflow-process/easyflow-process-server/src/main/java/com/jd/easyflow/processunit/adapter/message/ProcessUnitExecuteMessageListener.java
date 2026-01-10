package com.jd.easyflow.processunit.adapter.message;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.message.BaseMessageListener;
import com.jd.easyflow.message.Message;
import com.jd.easyflow.processunit.domain.model.vo.ProcessUnitExecuteMessage;
import com.jd.easyflow.processunit.domain.service.ProcessUnitService;
import com.jd.easyflow.utils.json.JSON;

/**
/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitExecuteMessageListener extends BaseMessageListener<ProcessUnitExecuteMessage> {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitExecuteMessageListener.class);


    @Autowired
    private ProcessUnitService processUnitService;

    @Override
    public boolean validate(ProcessUnitExecuteMessage obj, Message message) {
        return true;
    }

    @Override
    public void process(ProcessUnitExecuteMessage obj, Message message) {
        log.info("Process unit receive execute message:{}", JSON.toJSONString(obj));
        processUnitService.executeMessage(obj.getInstanceNo(), obj.getUnitCode(), obj.getBizNo());
    }

    public ProcessUnitService getProcessUnitService() {
        return processUnitService;
    }

    public void setProcessUnitService(ProcessUnitService processUnitService) {
        this.processUnitService = processUnitService;
    }
    
    

}
