package com.jd.easyflow.processunit.adapter.export;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.core.type.TypeReference;
import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.processunit.adapter.export.converter.PagerConverter;
import com.jd.easyflow.processunit.adapter.export.converter.ProcessUnitConverter;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateRes;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteRes;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecutionDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecutionQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitInstanceQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateRes;
import com.jd.easyflow.processunit.adapter.export.dto.ShardingInfoDTO;
import com.jd.easyflow.processunit.adapter.export.dto.ShardingInfoQueryReq;
import com.jd.easyflow.processunit.adapter.export.dto.ShutdownReq;
import com.jd.easyflow.processunit.adapter.export.dto.ShutdownRes;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteReq;
import com.jd.easyflow.processunit.domain.model.vo.SyncAfterCallRes;
import com.jd.easyflow.processunit.domain.model.vo.SyncBeforeCallRes;
import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteReq.VariableEntry;
import com.jd.easyflow.processunit.domain.model.vo.ExecuteReq;
import com.jd.easyflow.processunit.domain.model.vo.ExecuteRes;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.processunit.domain.service.ProcessUnitService;
import com.jd.easyflow.sharding.service.ShardingComputeResult;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitExportImpl implements ProcessUnitExport {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitExportImpl.class);


    @Autowired
    private ProcessUnitService processUnitService;
    @Autowired
    private ProcessUnitRepository processUnitRepository;
    
    @Action(code = "easyflow-processunit-syncBeforeCall", name = "syncBeforeCall")
    @Override
    public ExportResponse<com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallRes> syncBeforeCall(
            ExportRequest<com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallReq> request) {
        SyncBeforeCallRes res = processUnitService
                .syncBeforeCall(ProcessUnitConverter.INSTANCE.convert(request.getData()));
        return ExportResponse.build4Success(ProcessUnitConverter.INSTANCE.convert(res));
    }

    @Action(code = "easyflow-processunit-syncAfterCall", name = "syncAfterCall")
    @Override
    public ExportResponse<com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallRes> syncAfterCall(
            ExportRequest<com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallReq> request) {
        SyncAfterCallRes res = processUnitService
                .syncAfterCall(ProcessUnitConverter.INSTANCE.convert(request.getData()));
        return ExportResponse.build4Success(ProcessUnitConverter.INSTANCE.convert(res));
    }
    
    @Action(code = "easyflow-processunit-execute", name = "execute")
    @Override
    public ExportResponse<ProcessUnitExecuteRes> execute(ExportRequest<ProcessUnitExecuteReq> request) {
        ProcessUnitExecuteReq req = request.getData();
        if (req.getProcessUnitInstanceNo() == null && (req.getBizNo() == null || req.getUnitCode() == null)) {
            return ExportResponse.build4Failed(ExportResponseCode.INVALID);
        }
        ExecuteReq executeReq = new ExecuteReq();
        executeReq.setUnitInstanceNo(req.getProcessUnitInstanceNo());
        executeReq.setUnitCode(req.getUnitCode());
        executeReq.setBizNo(req.getBizNo());
        ExecuteRes executeRes = processUnitService.execute(executeReq);
        ProcessUnitExecuteRes res = new ProcessUnitExecuteRes();
        res.setResult(executeRes.getResult());
        res.setResponseContent(executeRes.getResponseContent());
        return ExportResponse.build4Success(res);
    }

    @Action(code = "easyflow-processunit-batchExecute", name = "batchExecute")
    @Override
    public ExportResponse<Void> batchExecute(ExportRequest<Map<String, String>> request) {
        Map<String, String> params = request.getData();
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
        return ExportResponse.build4Success();
    }

    @Action(code = "easyflow-processunit-create", name = "create")
    @Override
    public ExportResponse<ProcessUnitCreateRes> create(ExportRequest<ProcessUnitCreateReq> req) {
        log.info("Process unit create start, {}", JSON.toJSONString(req));
        ProcessUnitCreateRes res = new ProcessUnitCreateRes();
        String instanceNo = processUnitService.create(req.getData());
        res.setProcessUnitInstanceNo(instanceNo);
        log.info("Process unit create end");
        return ExportResponse.build4Success(res);

    }

    @Action(code = "easyflow-processunit-getByProcessInstanceNo", name = "getByProcessInstanceNo")
    @Override
    public ExportResponse<ProcessUnitInstanceDTO> getByProcessInstanceNo(ExportRequest<String> req) {
        ProcessUnitInstanceEntity entity = processUnitRepository.getInstance(req.getData());
        ProcessUnitInstanceDTO dto = ProcessUnitConverter.INSTANCE.convert(entity);
        return ExportResponse.build4Success(dto);
    }

    @Action(code = "easyflow-processunit-getByBizNoAndProcessUnitCode", name = "getByBizNoAndProcessUnitCode")
    @Override
    public ExportResponse<ProcessUnitInstanceDTO> getByBizNoAndProcessUnitCode(
            ExportRequest<ProcessUnitInstanceQueryReq> req) {
        ProcessUnitInstanceQueryReq query = req.getData();
        ProcessUnitInstanceEntity entity = processUnitRepository
                .getInstanceByUnitCodeAndBizNo(query.getProcessUnitCode(), query.getBizNo());
        ProcessUnitInstanceDTO dto = ProcessUnitConverter.INSTANCE.convert(entity);
        return ExportResponse.build4Success(dto);
    }

    @Action(code = "easyflow-processunit-updateProcessUnitInstanceByInstanceNoSelective", name = "updateProcessUnitInstanceByInstanceNoSelective")
    @Override
    public ExportResponse<Object> updateProcessUnitInstanceByInstanceNoSelective(ExportRequest<ProcessUnitInstanceDTO> req) {
        ProcessUnitInstanceEntity entity = processUnitRepository.getInstance(req.getData().getInstanceNo(),
                req.getData().getProcessUnitCode(), req.getData().getBizNo());
        if (entity == null) {
            return ExportResponse.build4Failed(ExportResponseCode.DATA_EMPTY);
        }
        req.getData().setProcessUnitCode(entity.getProcessUnitCode());
        req.getData().setBizNo(entity.getBizNo());
        processUnitRepository.updateInstanceByInstanceNoSelective(ProcessUnitConverter.INSTANCE.convert(req.getData()));
        return ExportResponse.build4Success();
    }

    @Action(code = "easyflow-processunit-update", name = "update")
    @Override
    public ExportResponse<ProcessUnitUpdateRes> update(ExportRequest<ProcessUnitUpdateReq> req) {
        log.info("Process unit update start, {}", JSON.toJSONString(req));
        ProcessUnitUpdateRes res = new ProcessUnitUpdateRes();
        processUnitService.update(req.getData());
        log.info("Process unit update end");
        return ExportResponse.build4Success(res);
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
        if (params.get("nextAutoRunTimeMaxInterval") != null) {
            req.setNextAutoRunTimeMaxInterval(Integer.parseInt((String) params.get("nextAutoRunTimeMaxInterval")));
        }
        if (params.get("variableList") != null) {
            List<VariableEntry> variableList = JSON.parseArray(params.get("variableList"), VariableEntry.class);
            req.setVariableList(variableList);
        }
        return req;
    }
    
    @Action(code = "coffee-processunit-queryByUnitCodeAndBizNoPrefix", name = "queryByUnitCodeAndBizNoPrefix")
    @Override
    public ExportResponse<List<ProcessUnitInstanceDTO>> queryByUnitCodeAndBizNoPrefix(
            ExportRequest<ProcessUnitInstanceQueryReq> req) {
        ProcessUnitInstanceQueryReq query = req.getData();
        AssertUtils.isNotNull(query.getBizNo(),"bizNo is empty");
        String bizNo = query.getBizNo();
        int length = bizNo.length();
        if (length < 20) {
            throw new RuntimeException("queryByUnitCodeAndBizNoPrefix.bizNo length illegal" + bizNo);
        }
        List<ProcessUnitInstanceEntity> list = processUnitRepository
                .queryByUnitCodeAndBizNoPrefix(query.getProcessUnitCode(), query.getBizNo());
        List<ProcessUnitInstanceDTO> result = list.stream().map(entity -> ProcessUnitConverter.INSTANCE.convert(entity)).collect(Collectors.toList());
        return ExportResponse.build4Success(result);
    }
    
    
    @Action(code = "easyflow-processunit-pagerQueryProcessUnitExecution", name = "pagerQueryProcessUnitExecution")
    @Override
    public ExportResponse<PagerResult> pagerQueryProcessUnitExecution(ExportRequest<PagerCondition> req) {
        com.jd.easyflow.common.dto.pager.PagerCondition condition = PagerConverter.INSTANCE.convert(req.getData());
        com.jd.easyflow.common.dto.pager.PagerResult result = processUnitService
                .pagerQueryProcessUnitExecution(condition);
        PagerResult exportResult = new PagerResult();
        exportResult.setCount(result.getCount());
        exportResult.setList(ProcessUnitConverter.INSTANCE.convertExecutionList(result.getList()));
        return ExportResponse.build4Success(exportResult);
    }

    @Action(code = "easyflow-processunit-getExecutionByExecutionNo", name = "getExecutionByExecutionNo")
    @Override
    public ExportResponse<ProcessUnitExecutionDTO> getExecutionByExecutionNo(
            ExportRequest<ProcessUnitExecutionQueryReq> req) {
        ProcessUnitExecutionQueryReq query = req.getData();
        ProcessUnitExecutionEntity execution = processUnitRepository.getExecution(query.getExecutionNo(), query.getProcessUnitCode(), query.getBizNo());
        ProcessUnitExecutionDTO dto = ProcessUnitConverter.INSTANCE.convert(execution);
        return ExportResponse.build4Success(dto);
    }

    @Action(code = "easyflow-processunit-queryProcessUnitList", name = "queryProcessUnitList")
    @Override
    public ExportResponse<List<ProcessUnitDTO>> queryProcessUnitList(ExportRequest<ProcessUnitQueryReq> req) {
        AssertUtils.isNotNull(req.getData());
        List<ProcessUnitEntity> processUnitList = processUnitRepository.findAllProcessUnitList();
        List<ProcessUnitDTO> dtoList = ProcessUnitConverter.INSTANCE.convertProcessUnitList(processUnitList);
        return ExportResponse.build4Success(dtoList);
    }
    
    @Action(code = "easyflow-processunit-getProcessUnit", name = "getProcessUnit")
    public ExportResponse<ProcessUnitDTO> getProcessUnit(ExportRequest<String> req) {
        AssertUtils.isNotNull(req.getData());
        ProcessUnitEntity entity = processUnitRepository.getProcessUnitByCode(req.getData());
        ProcessUnitDTO dto = ProcessUnitConverter.INSTANCE.convert(entity);
        if (dto != null) {
            dto.setConfig(JSON.toJSONString(entity.getConfigMap()));
        }
        return ExportResponse.build4Success(dto);
    }
    
    @Action(code = "easyflow-processunit-queryShardingInfo", name = "queryShardingInfo")
    public ExportResponse<ShardingInfoDTO> queryShardingInfo(ExportRequest<ShardingInfoQueryReq> req) {
        AssertUtils.isNotNull(req.getData());
        ShardingInfoQueryReq query = req.getData();
        AssertUtils.isNotNull(query.getUnitCode());
        AssertUtils.isNotNull(query.getBizNo());
        ShardingComputeResult info = processUnitRepository.computeShardInfo(query.getUnitCode(), query.getBizNo());
        return ExportResponse.build4Success(ProcessUnitConverter.INSTANCE.convert(info));
    }
    
    @Action(code = "easyflow-processunit-pagerQueryProcessUnitInstance", name = "pagerQueryProcessUnitInstance")
    @Override
    public ExportResponse<PagerResult> pagerQueryProcessUnitInstance(ExportRequest<PagerCondition> req) {
        com.jd.easyflow.common.dto.pager.PagerCondition condition = PagerConverter.INSTANCE.convert(req.getData());
        com.jd.easyflow.common.dto.pager.PagerResult result = processUnitService
                .pagerQueryProcessUnitInstance(condition);
        PagerResult exportResult = new PagerResult();
        exportResult.setCount(result.getCount());
        exportResult.setList(ProcessUnitConverter.INSTANCE.convertInstanceList(result.getList()));
        return ExportResponse.build4Success(exportResult);
    }

    @Action(code = "coffee-processunit-shutdown", name = "shutdown")
    @Override
    public ExportResponse<ShutdownRes> shutdown(ExportRequest<ShutdownReq> req) {
        processUnitService.shutdown();
        return ExportResponse.build4Success();
    }

    public ProcessUnitService getProcessUnitService() {
        return processUnitService;
    }

    public void setProcessUnitService(ProcessUnitService processUnitService) {
        this.processUnitService = processUnitService;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }
    
    

}
