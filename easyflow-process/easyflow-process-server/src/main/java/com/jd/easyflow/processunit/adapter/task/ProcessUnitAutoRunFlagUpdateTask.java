package com.jd.easyflow.processunit.adapter.task;


import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteReq;
import com.jd.easyflow.processunit.domain.service.ProcessUnitService;

/**
 * @author liyuliang5
 */
public class ProcessUnitAutoRunFlagUpdateTask {

    private static final Logger log = LoggerFactory.getLogger(ProcessUnitAutoRunFlagUpdateTask.class);
    
    @Autowired
    private ProcessUnitService processUnitService;

    /**
     * when type batch : processUnitCodeList,bizNoList,instanceNoList
     * when type auto : processUnitCodeList,nextAutoRunTimeStart,nextAutoRunTimEnd
     * @param params
     */
    public void execute(Map<String, String> params) {
        log.info("Process unit autoRunFlag update start, param:{}", params);
        BatchExecuteReq req = new BatchExecuteReq();
        req.setType(params.get("type"));
        if (params.get("processUnitCodeList") != null) {
            req.setProcessUnitCodeList(Arrays.asList(((String) params.get("processUnitCodeList")).split(",")));
        }
        if (params.get("nextAutoRunTimeStart") != null) {
            try {
                req.setNextAutoRunTimeStart(
                        new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse((String) params.get("nextAutoRunTimeStart")));
            } catch (ParseException e) {
                throw new RuntimeException("Date parse error", e);
            }
        }
        if (params.get("nextAutoRunTimeEnd") != null) {
            try {
                req.setNextAutoRunTimeEnd(
                        new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse((String) params.get("nextAutoRunTimeEnd")));
            } catch (ParseException e) {
                throw new RuntimeException("Date parse error", e);
            }
        }
        if (params.get("instanceNoList") != null) {
            req.setInstanceNoList(Arrays.asList(((String) params.get("instanceNoList")).split(",")));
        }
        if (params.get("bizNoList") != null) {
            req.setBizNoList(Arrays.asList(((String) params.get("bizNoList")).split(",")));
        }
        
        processUnitService.updateAutoRunFlag(req);
        log.info("Process unit autoRunFlag update end");
    }

    public ProcessUnitService getProcessUnitService() {
        return processUnitService;
    }

    public void setProcessUnitService(ProcessUnitService processUnitService) {
        this.processUnitService = processUnitService;
    }
    
    
}
