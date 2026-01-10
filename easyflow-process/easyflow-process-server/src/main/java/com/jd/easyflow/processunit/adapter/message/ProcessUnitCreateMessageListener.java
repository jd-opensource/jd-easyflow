package com.jd.easyflow.processunit.adapter.message;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.alert.AlertUtil;
import com.jd.easyflow.message.BaseMessageListener;
import com.jd.easyflow.message.Message;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.domain.service.ProcessUnitService;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitCreateMessageListener extends BaseMessageListener<ProcessUnitCreateReq> {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitCreateMessageListener.class);


    @Autowired
    private ProcessUnitService processUnitService;

    @Override
    public boolean validate(ProcessUnitCreateReq obj, Message message) {
        return true;
    }

    @Override
    public void process(ProcessUnitCreateReq obj, Message message) {
        log.info("Process unit client receive create message:{}", JSON.toJSONString(obj));
        try {
            processUnitService.syncCreate(obj, null);
        } catch (Exception e) {
            AlertUtil.doAlert("processUnit-createMessage", "create process unit exception,", e, obj, obj.getProductCode());
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
