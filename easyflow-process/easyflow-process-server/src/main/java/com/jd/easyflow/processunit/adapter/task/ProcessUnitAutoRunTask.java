package com.jd.easyflow.processunit.adapter.task;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.core.type.TypeReference;
import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteReq;
import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteReq.VariableEntry;
import com.jd.easyflow.processunit.domain.service.ProcessUnitService;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 * 
 */
public class ProcessUnitAutoRunTask {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitAutoRunTask.class);


    @Autowired
    private ProcessUnitService processUnitService;

    public void execute(Map<String, String> params) {
        log.info("Process unit batch execute start:{}", params);
        List<BatchExecuteReq> reqList = new ArrayList<>();
        BatchExecuteReq sharedExecuteReq = buildExecutionReq(params);
        reqList.add(sharedExecuteReq);

        List<Map<String, Object>> instanceContextList = JSON.parseObject(params.get("instanceContextList"),
                new TypeReference<List<Map<String, Object>>>() {
                });
        if (instanceContextList == null) {
            instanceContextList = new ArrayList<>();
        }
        for (Map<String, Object> map : instanceContextList) {
            BatchExecuteReq exclusiveExecuteReq = buildExecutionReq(map);
            reqList.add(exclusiveExecuteReq);
        }
        for (BatchExecuteReq batchExecuteReq : reqList) {
            processUnitService.batchExecute(batchExecuteReq);
        }
        log.info("Process unit batch execute end");
    }

    private BatchExecuteReq buildExecutionReq(Map params) {
        BatchExecuteReq req = new BatchExecuteReq();
        req.setType((String) params.get("type"));
        Object requestContext = params.get("context");
        req.setRequestContext(JSON.parseObject(requestContext, Map.class));
        if (params.get("instanceNoList") != null) {
            req.setInstanceNoList(Arrays.asList(((String) params.get("instanceNoList")).split(",")));
        }
        if (params.get("bizNoList") != null) {
            req.setBizNoList(Arrays.asList(((String) params.get("bizNoList")).split(",")));
        }
        if (params.get("processUnitCodeList") != null) {
            req.setProcessUnitCodeList(Arrays.asList(((String) params.get("processUnitCodeList")).split(",")));
        }
        if (params.get("excludeProcessUnitCodeList") != null) {
            req.setExcludeProcessUnitCodeList(Arrays.asList(((String) params.get("excludeProcessUnitCodeList")).split(",")));
        }
        if (params.get("productCodeList") != null) {
            req.setProductCodeList(Arrays.asList(((String) params.get("productCodeList")).split(",")));
        }
        if (params.get("excludeProductCodeList") != null) {
            req.setExcludeProductCodeList(Arrays.asList(((String) params.get("excludeProductCodeList")).split(",")));
        }
        if (params.get("resultList") != null) {
            req.setResultList(Arrays.asList(((String) params.get("resultList")).split(",")));
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
        if (params.get("nextAutoRunTimeMaxInterval") != null) {
            req.setNextAutoRunTimeMaxInterval(Integer.parseInt((String) params.get("nextAutoRunTimeMaxInterval")));
        }
        if (params.get("variableList") != null) {
            List<VariableEntry> variableList = JSON.parseArray(params.get("variableList"), VariableEntry.class);
            req.setVariableList(variableList);
        }
        
        req.setBatchRunPolicy((String) params.get("batchRunPolicy")); 
        if (params.get("threadNum") != null) {
            req.setThreadNum(Integer.parseInt((String) params.get("threadNum")));
        }
        
        
        return req;
    }

    public ProcessUnitService getProcessUnitService() {
        return processUnitService;
    }

    public void setProcessUnitService(ProcessUnitService processUnitService) {
        this.processUnitService = processUnitService;
    }
    
    

}
