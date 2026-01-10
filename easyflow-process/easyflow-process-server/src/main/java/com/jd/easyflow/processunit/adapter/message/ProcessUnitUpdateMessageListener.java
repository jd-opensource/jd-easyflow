package com.jd.easyflow.processunit.adapter.message;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.alert.AlertUtil;
import com.jd.easyflow.message.BaseMessageListener;
import com.jd.easyflow.message.Message;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateReq;
import com.jd.easyflow.processunit.domain.service.ProcessUnitService;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitUpdateMessageListener extends BaseMessageListener<ProcessUnitUpdateReq> {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitUpdateMessageListener.class);


    @Autowired
    private ProcessUnitService processUnitService;

    @Override
    public boolean validate(ProcessUnitUpdateReq obj, Message message) {
        return true;
    }

    @Override
    public void process(ProcessUnitUpdateReq obj, Message message) {
        log.info("Process unit receive update message:{}", JSON.toJSONString(obj));
        try {
            processUnitService.doUpdate(obj);
        } catch (Exception e) {
            AlertUtil.doAlert("processUnit-updateMessage", "process unit update exception,", e, obj, obj.getProductCode());
            throw e;
        }
    }

    public ProcessUnitService getProcessUnitService() {
        return processUnitService;
    }

    public void setProcessUnitService(ProcessUnitService processUnitService) {
        this.processUnitService = processUnitService;
    }
    
    
}
