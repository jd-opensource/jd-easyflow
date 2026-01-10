package com.jd.easyflow.processunit.infrastructure.repository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;

import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;
import com.jd.easyflow.processunit.domain.model.vo.ProcessUnitInstanceKey;
import com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.processunit.infrastructure.converter.ProcessUnitConverter;
import com.jd.easyflow.processunit.infrastructure.persistence.mapper.ProcessUnitExecutionMapper;
import com.jd.easyflow.processunit.infrastructure.persistence.mapper.ProcessUnitInstanceMapper;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitExecution;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitInstance;
import com.jd.easyflow.sharding.CurrentShardInfo;
import com.jd.easyflow.sharding.config.ShardingConfigManager;
import com.jd.easyflow.sharding.service.ShardingComputeResult;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 *
 * @author liyuliang5
 * 
 */
public class ProcessUnitRepositoryImpl implements ProcessUnitRepository {

    private static final Logger log = LoggerFactory.getLogger(ProcessUnitRepositoryImpl.class);

    @Autowired
    protected ProcessUnitInstanceMapper processUnitInstanceMapper;
    @Autowired
    protected ProcessUnitExecutionMapper processUnitExecutionMapper;
    @Autowired
    private ProcessUnitConfigCache localCache;

    protected boolean shardingEnabled = false;

    protected ShardingService shardingService;
    
    protected DataSource dataSource;
    
    protected ShardingConfigManager shardingConfigManager;
    
    private int insertDuplicateIdLoopTimes = 500;
    
    private int insertDuplicateRetryTimes = 3;
    
    public void init() {

    }

    @Override
    public ProcessUnitEntity getProcessUnitByCode(String processUnitCode) {
        ProcessUnitEntity processUnit = localCache.getProcessUnit(processUnitCode);
        return processUnit;
    }

    @Override
    public List<ProcessUnitEntity> findAllProcessUnitList() {
        return localCache.getAllProcessUnitList();
    }

    @Override
    public ProcessUnitInstanceEntity getInstanceByUnitCodeAndBizNo(String unitCode, String bizNo) {
        if (!shardingEnabled) {
            ProcessUnitInstance instance = processUnitInstanceMapper.getByUnitCodeAndBizNo(unitCode, bizNo);
            return ProcessUnitConverter.INSTANCE.po2Entity(instance);
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            return shardingService.executeInDefaultShard(shard -> {
                ProcessUnitInstance instance = processUnitInstanceMapper.getByUnitCodeAndBizNo(unitCode, bizNo);
                return ProcessUnitConverter.INSTANCE.po2Entity(instance);
            });
        }
        if (unitCode == null || bizNo == null) {
            log.warn("getInstanceByUnitCodeAndBizNo:unitCode and bizNo can not both null, unitCode:{} bizNo:{}", unitCode, bizNo);
            return null;
        }
        return shardingService.execute(ShardingService.OP_TYPE_READ, unitCode, bizNo, (shard) -> {
            ProcessUnitInstance instance = processUnitInstanceMapper.getByUnitCodeAndBizNo(unitCode, bizNo);
            return ProcessUnitConverter.INSTANCE.po2Entity(instance);
        });
    }
    
    
    @Override
    public List<ProcessUnitInstanceEntity> queryByUnitCodeAndBizNoPrefix(String unitCode, String bizNo) {
        int length = bizNo.length();
        if (length < 20) {
            throw new RuntimeException("queryByUnitCodeAndBizNoPrefix.bizNo length illegal" + bizNo);
        }
        Function<CurrentShardInfo, List> function = shard -> {
            List<ProcessUnitInstance> instanceList = processUnitInstanceMapper.selectListByUnitCodeAndBizNoPrefix(unitCode, bizNo);
            List<ProcessUnitInstanceEntity> list = instanceList.stream().map(processUnitInstance -> ProcessUnitConverter.INSTANCE.po2Entity(processUnitInstance)).collect(Collectors.toList());
            return list;
        };
        
        if (! shardingEnabled) {
            return function.apply(null);
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            return shardingService.executeInDefaultShard(function);
        }
        if (unitCode != null) {
            return shardingService.findListInShardsOfGroup(unitCode, false, function);
        } else {
            return shardingService.findListInAllShards(function);
        }
        
    }

    @Override
    public ProcessUnitInstanceEntity getInstance(String instanceNo) {
        return getInstance(instanceNo, null, null);
    }

    @Override
    public ProcessUnitInstanceEntity getInstance(String instanceNo, String unitCode, String bizNo) {
        Function<CurrentShardInfo, ProcessUnitInstanceEntity> function = (shard) -> {
            if (instanceNo != null) {
                ProcessUnitInstance instance = processUnitInstanceMapper.getByInstanceNo(instanceNo);
                if (instance == null) {
                    return null;
                }
                if ((unitCode != null && !unitCode.equals(instance.getProcessUnitCode()))
                        || (bizNo != null && !bizNo.equals(instance.getBizNo()))) {
                    log.warn("instanceNo:{} unitCode:{} bizNo:{} state inconsistent", instanceNo, unitCode, bizNo);
                    return null;
                }
                return ProcessUnitConverter.INSTANCE.po2Entity(instance);
            } else if (unitCode != null && bizNo != null) {
                ProcessUnitInstance instance = processUnitInstanceMapper.getByUnitCodeAndBizNo(unitCode, bizNo);
                if (instance == null) {
                    return null;
                }
                return ProcessUnitConverter.INSTANCE.po2Entity(instance);
            } else {
                throw new IllegalArgumentException(
                        "Param illegal:instanceNo:" + instanceNo + " unitCode:" + unitCode + " bizNo:" + bizNo);
            }
        };
        if (!shardingEnabled) {
            return function.apply(null);
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            return shardingService.executeInDefaultShard(function);
        }
        if (unitCode != null && bizNo != null) {
            return shardingService.execute(shardingService.OP_TYPE_READ, unitCode, bizNo, function);
        }
        if (unitCode != null) {
            return shardingService.getOneInShardsOfGroup(unitCode, function);
        }
        return shardingService.getOneInAllShards(function);
    }

    @Override
    public void saveInstance(ProcessUnitInstanceEntity entity) {
        if (!shardingEnabled) {
            ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
            processUnitInstanceMapper.insert(instance);
            entity.setId(instance.getId());
            return;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            shardingService.executeInDefaultShard(shard -> {
                ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                processUnitInstanceMapper.insert(instance);
                entity.setId(instance.getId());
                return null;
            });
            return;
        }
        String idStr = CodeGenerateHelper.generateCode("PU-INSTANCE-ID", "");
        long id = Long.parseLong(idStr);
        try {
            shardingService.execute(ShardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                    shard -> {
                        ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                        instance.setId(id);
                        processUnitInstanceMapper.insert(instance);
                        entity.setId(instance.getId());
                        return null;
                    });
        } catch (DuplicateKeyException e) {
            log.error("Instance " + entity + " id " + idStr + " insert duplicate exception," + e.getMessage(), e);
            if (insertDuplicateRetryTimes < 1) {
                throw e;
            }
            int retryTimes = 0;
            while (true) {
                retryTimes++;
                for (int j = 0; j < insertDuplicateIdLoopTimes; j++) {
                    idStr = CodeGenerateHelper.generateCode("PU-INSTANCE-ID", "");
                }
                long newId = Long.parseLong(idStr);
                try {
                    shardingService.execute(ShardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                            shard -> {
                                ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                                instance.setId(newId);
                                processUnitInstanceMapper.insert(instance);
                                entity.setId(instance.getId());
                                return null;
                            });
                    log.info("Regenerated id:" + idStr + " retry insert success");
                    break;
                } catch (DuplicateKeyException e2) {
                    log.error("Record " + entity + " id " + idStr + " retry insert exception," + e.getMessage(), e);
                    if (retryTimes == insertDuplicateRetryTimes) {
                        throw e2;
                    }
                }
            }
        }
    }

    @Override
    public void updateInstance(ProcessUnitInstanceEntity entity) {
        if (!shardingEnabled) {
            ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
            processUnitInstanceMapper.updateByPrimaryKey(instance);
            return;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            shardingService.executeInDefaultShard(shard -> {
                ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                processUnitInstanceMapper.updateByPrimaryKey(instance);
                return null;
            });
            return;
        }
        shardingService.execute(ShardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                (shard) -> {
                    ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                    processUnitInstanceMapper.updateByInstanceNo(instance);
                    return null;
                });
    }

    
    @Override
    public void updateInstanceByInstanceNoSelective(ProcessUnitInstanceEntity entity) {
        if (!shardingEnabled) {
            ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
            processUnitInstanceMapper.updateByInstanceNoSelective(instance);
            return;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            shardingService.executeInDefaultShard(shard -> {
                ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                processUnitInstanceMapper.updateByInstanceNoSelective(instance);
                return null;
            });
            return;
        }
        shardingService.execute(shardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                (shard) -> {
                    ProcessUnitInstance instance = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                    processUnitInstanceMapper.updateByInstanceNoSelective(instance);
                    return null;
                });
    }

    @Override
    public List<String> findAsyncInstanceList(QueryAsyncInstanceVO query) {
        if (!shardingEnabled) {
            return processUnitInstanceMapper.findAsyncInstanceList(query);
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            return shardingService.executeInDefaultShard(shard -> {
                return processUnitInstanceMapper.findAsyncInstanceList(query);
            });
        }
        List<ProcessUnitInstanceKey> list = findAsyncInstanceKeyList(query);
        return list.stream().map(key -> key.getInstanceNo()).collect(Collectors.toList());
    }

    @Override
    public List<ProcessUnitInstanceKey> findAsyncInstanceKeyList(QueryAsyncInstanceVO query) {
        if (!shardingEnabled) {
            List<ProcessUnitInstanceKey> list = processUnitInstanceMapper.findAsyncInstanceKeyList(query);
            return list;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            return shardingService.executeInDefaultShard(shard -> {
                List<ProcessUnitInstanceKey> list = processUnitInstanceMapper.findAsyncInstanceKeyList(query);
                return list;
            });
        }
        if (query.getUnitCode() != null) {
            return shardingService.findListLoopShardsOfGroup(query.getUnitCode(), query.getMaxCount(), false, (shard) -> {
                List<ProcessUnitInstanceKey> list = processUnitInstanceMapper.findAsyncInstanceKeyList(query);
                return list; 
            });
        }
        return shardingService.findListLoopAllShards(query.getMaxCount(), (shard) -> {
            List<ProcessUnitInstanceKey> list = processUnitInstanceMapper.findAsyncInstanceKeyList(query);
            return list; 
        });
    }

    public ProcessUnitExecutionEntity getExecution(String executionNo) {
        return getExecution(executionNo, null, null);
    }

    @Override
    public ProcessUnitExecutionEntity getExecution(String executionNo, String unitCode, String bizNo) {
        Function<CurrentShardInfo, ProcessUnitExecutionEntity> function = shard -> {
            ProcessUnitExecution execution = processUnitExecutionMapper.getByExecutionNo(executionNo);
            if (execution == null) {
                return null;
            }
            if ((unitCode != null && !unitCode.equals(execution.getProcessUnitCode()))
                    || (bizNo != null && execution.getBizNo() != null && !bizNo.equals(execution.getBizNo()))) {
                log.warn("executionNo:{} unitCode:{} bizNo:{} state inconsistent", executionNo, unitCode, bizNo);
                return null;
            }
            return ProcessUnitConverter.INSTANCE.po2Entity(execution);
        };
        
        if (!shardingEnabled) {
            return function.apply(null);
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            return shardingService.executeInDefaultShard(function);
        }
        if (unitCode != null && bizNo != null) {
            return shardingService.execute(shardingService.OP_TYPE_READ, unitCode, bizNo, function);
        } 
        if (unitCode != null) {
            return shardingService.getOneInShardsOfGroup(unitCode, function);
        }
        return shardingService.getOneInAllShards(function);
    }

    @Override
    public void saveExecution(ProcessUnitExecutionEntity entity) {
        if (!shardingEnabled) {
            ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
            processUnitExecutionMapper.insert(execution);
            entity.setId(execution.getId());
            return;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            shardingService.executeInDefaultShard(shard -> {
                ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                processUnitExecutionMapper.insert(execution);
                entity.setId(execution.getId());
                return null;
            });
            return;
        }

        if (entity.getBizNo() == null) {
            entity.setBizNo("");
        }
        String idStr = CodeGenerateHelper.generateCode("PU-EXECUTION-ID", "");
        long id = Long.parseLong(idStr);
        try {
            shardingService.execute(shardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                    (shard) -> {
                        ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                        execution.setId(id);
                        processUnitExecutionMapper.insert(execution);
                        entity.setId(execution.getId());
                        return null;
                    });
        } catch (DuplicateKeyException e) {
            log.error("Execution " + entity + " id " + idStr + " insert duplicate excetion," + e.getMessage(), e);
            if (insertDuplicateRetryTimes < 1) {
                throw e;
            }
            int retryTimes = 0;
            while (true) {
                retryTimes++;
                for (int j = 0; j < insertDuplicateIdLoopTimes; j++) {
                    idStr = CodeGenerateHelper.generateCode("PU-EXECUTION-ID", "");
                }
                long newId = Long.parseLong(idStr);
                try {
                    shardingService.execute(shardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                            shard -> {
                                ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                                execution.setId(newId);
                                processUnitExecutionMapper.insert(execution);
                                entity.setId(execution.getId());
                                return null;
                            });
                    log.info("Execution regenereted id:" + idStr + " retry insert success");
                    break;
                } catch (DuplicateKeyException e2) {
                    log.error("Execution " + entity + " id " + idStr + " retry insert exception," + e.getMessage(), e);
                    if (retryTimes == insertDuplicateRetryTimes) {
                        throw e2;
                    }
                }
            }
        }
    }

    @Override
    public void updateExecution(ProcessUnitExecutionEntity entity) {
        if (!shardingEnabled) {
            ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
            processUnitExecutionMapper.updateByPrimaryKey(execution);
            return;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            shardingService.executeInDefaultShard(shard -> {
                ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                processUnitExecutionMapper.updateByPrimaryKey(execution);
                return null;
            });
            return;
        }
        if (entity.getBizNo() == null) {
            entity.setBizNo("");
        }
        shardingService.execute(shardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                (shard) -> {
                    ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                    processUnitExecutionMapper.updateByExecutionNo(execution);
                    return null;
                });
    }

    @Override
    public void updateExecutionByExecutionNo(ProcessUnitExecutionEntity entity) {
        if (!shardingEnabled) {
            ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
            processUnitExecutionMapper.updateByExecutionNo(execution);
            return;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            shardingService.executeInDefaultShard(shard -> {
                ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                processUnitExecutionMapper.updateByExecutionNo(execution);
                return null;
            });
            return;
        }

        if (entity.getBizNo() == null) {
            entity.setBizNo("");
        }
        shardingService.execute(shardingService.OP_TYPE_WRITE, entity.getProcessUnitCode(), entity.getBizNo(),
                (shard) -> {
                    ProcessUnitExecution execution = ProcessUnitConverter.INSTANCE.entity2Po(entity);
                    processUnitExecutionMapper.updateByExecutionNo(execution);
                    return null;
                });
    }

    @Override
    public PagerResult pagerQueryProcessUnitInstance(PagerCondition pagerQueryReq) {
        FieldEntry queryTypeField = pagerQueryReq.getField("queryType");
        FieldEntry instanceNoField = pagerQueryReq.getField("instanceNo");
        FieldEntry unitCodeField = pagerQueryReq.getField("processUnitCode");
        FieldEntry bizNoField = pagerQueryReq.getField("bizNo");
        FieldEntry createdDateStartField = pagerQueryReq.getField("createdDateStart");
        FieldEntry createdDateEndField = pagerQueryReq.getField("createdDateEnd");
        FieldEntry resultListField = pagerQueryReq.getField("resultList");
        boolean usingSlaveDb = Boolean.TRUE.equals(pagerQueryReq.getExtData("_usingSlaveDb"));        

        String queryType = queryTypeField == null ? null : (String) queryTypeField.getValue();
        String instanceNo = instanceNoField == null ? null : (String) instanceNoField.getValue();
        String unitCode = unitCodeField == null ? null : (String) unitCodeField.getValue();
        String bizNo = bizNoField == null ? null : (String) bizNoField.getValue();
        String createdDateStart = createdDateStartField == null ? null : (String) createdDateStartField.getValue();
        String createdDateEnd = createdDateEndField == null ? null : (String) createdDateEndField.getValue();
        String[] resultList = resultListField == null ? null : (String[]) resultListField.getValue();

        PagerResult result = new PagerResult<>();
        if (ProcessUnitConstants.INSTANCE_QUERY_TYPE_SINGLE.equals(queryType)) {

            if (instanceNo == null && (unitCode == null || bizNo == null)) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), " param illegal, instanceNo is null and unitCode or bizNo is null");
            }
            ProcessUnitInstanceEntity entity = null;
            if (instanceNo != null) {
                entity = getInstance(instanceNo, unitCode, bizNo);
            } else {
                entity = getInstanceByUnitCodeAndBizNo(unitCode, bizNo);
            }
            if (entity != null) {
                if ((instanceNo != null && !instanceNo.equals(entity.getInstanceNo()))
                        || (unitCode != null && !unitCode.equals(entity.getProcessUnitCode()))
                        || (bizNo != null && !bizNo.equals(entity.getBizNo()))) {
                    entity = null;
                }
            }
            if (entity == null) {
                result.setList(new ArrayList<>());
                result.setCount(0L);
            } else {
                result.setList(Arrays.asList(entity));
                result.setCount(1L);
            }
        } else if (ProcessUnitConstants.INSTANCE_QUERY_TYPE_CREATED_DATE_RANGE.equals(queryType)) {
            if (createdDateStart == null || createdDateEnd == null) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), " Param illegal, both createdDateStart and createdDateEnd are null");
            }
            if (!shardingEnabled) {
                List<ProcessUnitInstance> list = processUnitInstanceMapper
                        .selectProcessUnitInstanceByPageCondition(pagerQueryReq);
                long count = processUnitInstanceMapper.countProcessUnitInstanceByPageCondition(pagerQueryReq);
                result.setList(ProcessUnitConverter.INSTANCE.instancePoList2EntityList(list));
                result.setCount(count);
                return result;
            }
            if (shardingConfigManager.isFixDefaultShard()) {
                return shardingService.executeInDefaultShard(shard -> {
                    List<ProcessUnitInstance> list = processUnitInstanceMapper
                            .selectProcessUnitInstanceByPageCondition(pagerQueryReq);
                    long count = processUnitInstanceMapper.countProcessUnitInstanceByPageCondition(pagerQueryReq);
                    result.setList(ProcessUnitConverter.INSTANCE.instancePoList2EntityList(list));
                    result.setCount(count);
                    return result;
                });
            }
            pagerQueryReq.addSortField("id", 1, "asc");

            long count = 0;
            List<ProcessUnitInstance> list = null;
            if (unitCode != null) {
                count = shardingService.countInShardsOfGroup(unitCode, usingSlaveDb,shard -> {
                    long countOfShard = processUnitInstanceMapper
                            .countProcessUnitInstanceByPageCondition(pagerQueryReq);
                    return countOfShard;
                });
                list = shardingService.findListInShardsOfGroup(unitCode, usingSlaveDb, shard -> {
                    List<ProcessUnitInstance> listOfShard = processUnitInstanceMapper
                            .selectProcessUnitInstanceByPageCondition(pagerQueryReq);
                    return listOfShard;
                });
            } else {
                count = shardingService.countInAllShards(usingSlaveDb, shard -> {
                    long countOfShard = processUnitInstanceMapper
                            .countProcessUnitInstanceByPageCondition(pagerQueryReq);
                    return countOfShard;
                });
                list = shardingService.findListInAllShards(usingSlaveDb, shard -> {
                    List<ProcessUnitInstance> listOfShard = processUnitInstanceMapper
                            .selectProcessUnitInstanceByPageCondition(pagerQueryReq);
                    return listOfShard;
                });
            }
            Collections.sort(list, (instance1, instance2) -> {
                return instance1.getId().compareTo(instance2.getId());
            });
            if (list.size() > pagerQueryReq.getPageSize()) {
                list = list.subList(0, pagerQueryReq.getPageSize());
            }
            result.setList(ProcessUnitConverter.INSTANCE.instancePoList2EntityList(list));
            result.setCount(count);
        } else {
            throw new UnsupportedOperationException("Unsupported query type:" + queryType);
        }
        return result;
    }

    @Override
    public PagerResult pagerQueryProcessUnitExecution(PagerCondition pagerQueryReq) {
        FieldEntry queryTypeField = pagerQueryReq.getField("queryType");
        FieldEntry unitCodeField = pagerQueryReq.getField("processUnitCode");
        FieldEntry bizNoField = pagerQueryReq.getField("bizNo");
        FieldEntry instanceNoField = pagerQueryReq.getField("instanceNo");
        FieldEntry requestTimeStartField = pagerQueryReq.getField("requestTimeStart");
        FieldEntry requestTimeEndField = pagerQueryReq.getField("requestTimeEnd");
        FieldEntry elapseTimeStartField = pagerQueryReq.getField("elapseTimeStart");
        FieldEntry elapseTimeEndField = pagerQueryReq.getField("elapseTimeEnd");
        FieldEntry resultListField = pagerQueryReq.getField("resultList");
        
        boolean usingSlaveDb = Boolean.TRUE.equals(pagerQueryReq.getExtData("_usingSlaveDb"));        

        String queryType = queryTypeField == null ? null : (String) queryTypeField.getValue();
        String instanceNo = instanceNoField == null ? null : (String) instanceNoField.getValue();
        String unitCode = unitCodeField == null ? null : (String) unitCodeField.getValue();
        String bizNo = bizNoField == null ? null : (String) bizNoField.getValue();
        String requestTimeStart = requestTimeStartField == null ? null : (String) requestTimeStartField.getValue();
        String requestTimeEnd = requestTimeEndField == null ? null : (String) requestTimeEndField.getValue();
        List<String> resultList = resultListField == null ? null : (List<String>) resultListField.getValue();

        PagerResult result = new PagerResult<>();
        if (ProcessUnitConstants.EXECUTION_QUERY_TYPE_INSTANCE.equals(queryType)) {
            if (instanceNo == null && (unitCode == null || bizNo == null)) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "param illegal, instanceNo is null and unitCode or bizNo is null");
            }
            if (!shardingEnabled) {
                List<ProcessUnitExecution> list = processUnitExecutionMapper
                        .selectProcessUnitExecutionByPageCondition(pagerQueryReq);
                long count = processUnitExecutionMapper.countProcessUnitExecutionByPageCondition(pagerQueryReq);
                result.setList(ProcessUnitConverter.INSTANCE.executionPoList2EntityList(list));
                result.setCount(count);
            } else if (shardingConfigManager.isFixDefaultShard()) {
                shardingService.executeInDefaultShard(shard -> {
                    List<ProcessUnitExecution> list = processUnitExecutionMapper
                            .selectProcessUnitExecutionByPageCondition(pagerQueryReq);
                    long count = processUnitExecutionMapper.countProcessUnitExecutionByPageCondition(pagerQueryReq);
                    result.setList(ProcessUnitConverter.INSTANCE.executionPoList2EntityList(list));
                    result.setCount(count);
                    return result;
                });
            } else {
                if (unitCode == null || bizNo == null) {
                    ProcessUnitInstanceEntity instance = getInstance(instanceNo);
                    if (instance == null) {
                        log.warn("instance:" + instanceNo + " not exists");
                        return new PagerResult(0L, new ArrayList());
                    }
                    if (unitCode == null) {
                        unitCode = instance.getProcessUnitCode();
                    } else if (!Objects.equals(unitCode, instance.getProcessUnitCode())) {
                        log.warn("unitCode:" + unitCode + " and instance " + instance + " unitCode not equals");
                        return new PagerResult(0L, new ArrayList());
                    }
                    if (bizNo == null) {
                        bizNo = instance.getBizNo();
                    } else if (!Objects.equals(bizNo, instance.getBizNo())) {
                        log.warn("bizNo:" + bizNo + " and instance " + instance + " bizNo not equals");
                        return new PagerResult(0L, new ArrayList());
                    }
                }
                shardingService.execute(shardingService.OP_TYPE_READ, unitCode, bizNo, usingSlaveDb, (shard) -> {
                    List<ProcessUnitExecution> list = processUnitExecutionMapper
                            .selectProcessUnitExecutionByPageCondition(pagerQueryReq);
                    long count = processUnitExecutionMapper.countProcessUnitExecutionByPageCondition(pagerQueryReq);
                    result.setList(ProcessUnitConverter.INSTANCE.executionPoList2EntityList(list));
                    result.setCount(count);
                    return null;
                });
            }
        } else if (ProcessUnitConstants.EXECUTION_QUERY_TYPE_REQUEST_TIME_RANGE.equals(queryType)) {
            if (requestTimeStart == null || requestTimeEnd == null) {
                throw new UserException(ExportResponseCode.INVALID.getCode(), "  Param illegal, both createdDateStart and createdDateEnd are null");
            }
            if (!shardingEnabled) {
                List<ProcessUnitExecution> list = processUnitExecutionMapper
                        .selectProcessUnitExecutionByPageCondition(pagerQueryReq);
                long count = processUnitExecutionMapper.countProcessUnitExecutionByPageCondition(pagerQueryReq);
                result.setList(ProcessUnitConverter.INSTANCE.executionPoList2EntityList(list));
                result.setCount(count);
            } else if (shardingConfigManager.isFixDefaultShard()) {
                shardingService.executeInDefaultShard(shard -> {
                    List<ProcessUnitExecution> list = processUnitExecutionMapper
                            .selectProcessUnitExecutionByPageCondition(pagerQueryReq);
                    long count = processUnitExecutionMapper.countProcessUnitExecutionByPageCondition(pagerQueryReq);
                    result.setList(ProcessUnitConverter.INSTANCE.executionPoList2EntityList(list));
                    result.setCount(count);
                    return null;
                });
            } else {
                pagerQueryReq.addSortField("id", 1, "asc");

                long count = 0;
                List<ProcessUnitExecution> list = null;
                if (unitCode != null) {
                    count = shardingService.countInShardsOfGroup(unitCode, usingSlaveDb, shard -> {
                        long countOfShard = processUnitExecutionMapper
                                .countProcessUnitExecutionByPageCondition(pagerQueryReq);
                        return countOfShard;
                    });
                    list = shardingService.findListInShardsOfGroup(unitCode, usingSlaveDb, shard -> {
                        List<ProcessUnitExecution> listOfShard = processUnitExecutionMapper
                                .selectProcessUnitExecutionByPageCondition(pagerQueryReq);
                        return listOfShard;
                    });
                } else {
                    count = shardingService.countInAllShards(usingSlaveDb, shard -> {
                        long countOfShard = processUnitExecutionMapper
                                .countProcessUnitExecutionByPageCondition(pagerQueryReq);
                        return countOfShard;
                    });
                    list = shardingService.findListInAllShards(usingSlaveDb, shard -> {
                        List<ProcessUnitExecution> listOfShard = processUnitExecutionMapper
                                .selectProcessUnitExecutionByPageCondition(pagerQueryReq);
                        return listOfShard;
                    });
                }
                Collections.sort(list, (instance1, instance2) -> {
                    return instance1.getId().compareTo(instance2.getId());
                });
                if (list.size() > pagerQueryReq.getPageSize()) {
                    list = list.subList(0, pagerQueryReq.getPageSize());
                }
                result.setList(ProcessUnitConverter.INSTANCE.executionPoList2EntityList(list));
                result.setCount(count);
            }

        }
        return result;
    }
    
    private List<Object[]> loopUpdateSqlInAllShard(String sql) {
        JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
        return shardingService.loopExecuteInAllShards(CurrentShardInfo -> {
            Integer count = jdbcTemplate.update(sql);
            return new Object[] {CurrentShardInfo, count};
        }, list -> {
            return list;
        });
    }
    
    private List<Object[]> loopQuerySqlInAllShard(String sql) {
        JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
        return shardingService.loopExecuteInAllShards(CurrentShardInfo -> {
            List<Map<String, Object>> list = jdbcTemplate.queryForList(sql);
            return new Object[] {CurrentShardInfo, list};
        }, list -> {
            return list;
        });
    }
    
    public boolean isShardingEnabled() {
        return shardingEnabled;
    }

    public void setShardingEnabled(boolean shardingEnabled) {
        this.shardingEnabled = shardingEnabled;
    }


    @Override
    public ShardingComputeResult computeShardInfo(String unitCode, String bizNo) {
        return shardingService.computeShardInfo(unitCode, bizNo);
    }
    
    public DataSource getDataSource() {
        return dataSource;
    }

    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    public ProcessUnitInstanceMapper getProcessUnitInstanceMapper() {
        return processUnitInstanceMapper;
    }

    public void setProcessUnitInstanceMapper(ProcessUnitInstanceMapper processUnitInstanceMapper) {
        this.processUnitInstanceMapper = processUnitInstanceMapper;
    }

    public ProcessUnitExecutionMapper getProcessUnitExecutionMapper() {
        return processUnitExecutionMapper;
    }

    public void setProcessUnitExecutionMapper(ProcessUnitExecutionMapper processUnitExecutionMapper) {
        this.processUnitExecutionMapper = processUnitExecutionMapper;
    }

    public ProcessUnitConfigCache getLocalCache() {
        return localCache;
    }

    public void setLocalCache(ProcessUnitConfigCache localCache) {
        this.localCache = localCache;
    }

    public ShardingConfigManager getShardingConfigManager() {
        return shardingConfigManager;
    }

    public void setShardingConfigManager(ShardingConfigManager shardingConfigManager) {
        this.shardingConfigManager = shardingConfigManager;
    }

    public int getInsertDuplicateIdLoopTimes() {
        return insertDuplicateIdLoopTimes;
    }

    public void setInsertDuplicateIdLoopTimes(int insertDuplicateIdLoopTimes) {
        this.insertDuplicateIdLoopTimes = insertDuplicateIdLoopTimes;
    }

    public int getInsertDuplicateRetryTimes() {
        return insertDuplicateRetryTimes;
    }

    public void setInsertDuplicateRetryTimes(int insertDuplicateRetryTimes) {
        this.insertDuplicateRetryTimes = insertDuplicateRetryTimes;
    }

    public ShardingService getShardingService() {
        return shardingService;
    }

    public void setShardingService(ShardingService shardingService) {
        this.shardingService = shardingService;
    }
    
    
    
}
