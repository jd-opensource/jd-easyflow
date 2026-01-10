package com.jd.easyflow.processunit.adapter.task;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.processunit.domain.model.vo.ShardingMigrateContext;
import com.jd.easyflow.processunit.domain.service.ProcessUnitShardingToolService;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUnitShardingMigrateTask {

    private static final Logger log = LoggerFactory.getLogger(ProcessUnitShardingMigrateTask.class);

    @Autowired
    private ProcessUnitShardingToolService shardingToolService;

    public void execute(Map<String, String> params) {
        log.info("Input param:" + params);
        ShardingMigrateContext context = new ShardingMigrateContext();
        String unitCodeListStr = params.get("unitCodeList");
        if (unitCodeListStr == null || unitCodeListStr.isEmpty()) {
            log.error("unitCodeList must not be null");
            return;
        }
        context.setUnitCodeList(Arrays.asList(unitCodeListStr.split(",")));
        String createdDateStartStr = params.get("createdDateStart");
        String createdDateEndStr = params.get("createdDateEnd");
        try {
            if (createdDateStartStr != null) {
                context.setCreatedDateStart(new SimpleDateFormat("yyyyMMddHHmmss").parse(createdDateStartStr));
            }
            if (createdDateEndStr != null) {
                context.setCreatedDateEnd(new SimpleDateFormat("yyyyMMddHHmmss").parse(createdDateEndStr));
            }
        } catch (Exception e) {
            throw new EasyFlowException("createdDateStart or createdDateEnd format error, should be yyyyMMddHHmmss", e);
        }
        context.setMigrateExecution(! Boolean.FALSE.toString().equals(params.get("migrateExecution")));
        String threadCountStr = params.get("threadCount");
        context.setThreadCount(threadCountStr == null ? 1 : Integer.parseInt(threadCountStr));
        context.setLock(! Boolean.FALSE.toString().equals(params.get("lock")));
        context.setInstanceNo(params.get("instanceNo"));
        context.setNewId(Boolean.TRUE.toString().equals(params.get("newId")));
        String resultListStr = params.get("resultList");
        if (resultListStr != null) {
            List<String> resultList = Arrays.asList(resultListStr.split(","));
            context.setResultList(resultList);
        }
        
        shardingToolService.migrate(context);
    }

    public ProcessUnitShardingToolService getShardingToolService() {
        return shardingToolService;
    }

    public void setShardingToolService(ProcessUnitShardingToolService shardingToolService) {
        this.shardingToolService = shardingToolService;
    }
    
    
}
