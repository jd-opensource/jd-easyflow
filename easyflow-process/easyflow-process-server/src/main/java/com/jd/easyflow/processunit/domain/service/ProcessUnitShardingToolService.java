package com.jd.easyflow.processunit.domain.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;

import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.lock.Locker;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.vo.ShardingCompareContext;
import com.jd.easyflow.processunit.domain.model.vo.ShardingMigrateContext;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.processunit.infrastructure.persistence.mapper.ProcessUnitExecutionMapper;
import com.jd.easyflow.processunit.infrastructure.persistence.mapper.ProcessUnitInstanceMapper;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitExecution;
import com.jd.easyflow.processunit.infrastructure.persistence.po.ProcessUnitInstance;
import com.jd.easyflow.sharding.config.ShardGroupInfo;
import com.jd.easyflow.sharding.config.ShardingConfigManager;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 * 
 * 
 * @author liyuliang5
 */
public class ProcessUnitShardingToolService {

    private static final Logger log = LoggerFactory.getLogger(ProcessUnitShardingToolService.class);

    @Autowired
    private ProcessUnitInstanceMapper processUnitInstanceMapper;
    @Autowired
    private ProcessUnitExecutionMapper processUnitExecutionMapper;
    @Autowired
    private ProcessUnitRepository processUnitRepository;

    private ShardingService shardingService;

    private ShardingConfigManager shardingConfigManager;

    private Locker lockService;

    protected boolean migrateStopFlag;

    protected boolean compareStopFlag;

    public void migrate(ShardingMigrateContext context) {
        for (String unitCode : context.getUnitCodeList()) {
            log.info("Start migrate process unit:{}", unitCode);
            try {
                migrateOneUnit(unitCode, context);
                log.info("Process unit:{} migrate end", unitCode);
            } catch (Exception e) {
                log.error("Process unit:" + unitCode + " migrate exception " + e.getMessage(), e);
            }
        }
    }


    private void migrateOneUnit(String unitCode, ShardingMigrateContext context) {
        ProcessUnitEntity processUnit = processUnitRepository.getProcessUnitByCode(unitCode);
        ShardGroupInfo shardGroup = shardingConfigManager.getShardGroupMap().get(unitCode);
        List<String> shardList = shardGroup.getShardList();
        log.info("{} migrate shard list are:{}", unitCode, shardList);
        ExecutorService service = Executors.newFixedThreadPool(context.getThreadCount());
        List<Future> resultList = new ArrayList<Future>();
        shardList.forEach(shard -> {
            resultList.add(service.submit(() -> {
                migrateOneShard(unitCode, shard, shardGroup, context);
            }));
        });
        for (Future<Boolean> future : resultList) {
            try {
                future.get();
            } catch (InterruptedException | ExecutionException e) {
                log.error("Migrate task execution exception:" + e.getMessage(), e);
            }
        }
    }

    private void migrateOneShard(String unitCode, String shard, ShardGroupInfo shardGroup, ShardingMigrateContext context) {
        log.info("Start migrate unitCode:{} shard:{}", unitCode, shard);
        try {
            PagerCondition condition = new PagerCondition();
            condition.addField(new FieldEntry("processUnitCode", unitCode));
            if (context.getCreatedDateStart() != null) {
                condition.addField(new FieldEntry("createdDateStart", context.getCreatedDateStart()));
            }
            if (context.getCreatedDateEnd() != null) {
                condition.addField(new FieldEntry("createdDateEnd", context.getCreatedDateEnd()));
            }
            if (context.getInstanceNo() != null) {
                condition.addField(new FieldEntry("instanceNo", context.getInstanceNo()));
            }
            if (context.getResultList() != null) {
                condition.addField(new FieldEntry("resultList", context.getResultList()));
            }
            condition.addSortField("id", 0, "asc");
            condition.setPageSize(context.getPageSize());
            condition.setStart(-2);
            shardingService.executeInShard(shard, (shardInfo) -> {
                FieldEntry idStartEntry = new FieldEntry("idStart", -1);
                condition.addField(idStartEntry);
                int count = 0;
                while (true) {
                    List<ProcessUnitInstance> list = processUnitInstanceMapper
                            .selectProcessUnitInstanceByPageCondition(condition);
                    long lastId = list.size() == 0 ? -1 : list.get(list.size() - 1).getId();
                    for (ProcessUnitInstance instance : list) {
                        migrateOneInstance(instance, shardGroup, context);
                        if (migrateStopFlag) {
                            log.info("Migrate end");
                            return null;
                        }
                    }
                    count += list.size();
                    log.info("shard:{} migrate finished count:{}", shard, count);
                    if (list.size() == 0) {
                        return null;
                    }
                    idStartEntry.setValue(lastId);
                }
            });
        } catch (Exception e) {
            log.error("Migrate shard:" + shard + " exception", e);
            throw e;
        }
    }

    private void migrateOneInstance(ProcessUnitInstance instance, ShardGroupInfo shardGroup, ShardingMigrateContext context) {
        Runnable runnable = () -> {
                String slaveShard = shardingService.computeSlaveShard(instance.getProcessUnitCode(), instance.getBizNo(),
                        shardGroup);
            if (slaveShard == null) {
                throw new EasyFlowException(instance.getProcessUnitCode() + " slaveShard not configured");
            }
            shardingService.executeInShard(slaveShard, (shardInfo) -> {
                try {
                    if (context.isNewId()) {
                        String idStr = CodeGenerateHelper.generateCode("PU-INSTANCE-ID", "");
                        instance.setId(Long.parseLong(idStr));
                    }
                    processUnitInstanceMapper.insertForMigration(instance);
                } catch (DuplicateKeyException e) {
                    log.warn(instance.getId() + "-" + instance.getInstanceNo() + "-" + instance.getProcessUnitCode()
                            + "-" + instance.getBizNo() + " insert duplicate exception");
                    processUnitInstanceMapper.updateForMigration(instance);
                    
                }
                return null;
            });
            if (context.isMigrateExecution()) {
                List<ProcessUnitExecution> executionList = processUnitExecutionMapper
                        .selectProcessUnitExecution(Arrays.asList(instance.getInstanceNo()));
                for (ProcessUnitExecution execution : executionList) {
                    shardingService.executeInShard(slaveShard, (shardInfo) -> {
                        if (context.isNewId()) {
                            String idStr = CodeGenerateHelper.generateCode("PU-EXECUTION-ID", "");
                            execution.setId(Long.parseLong(idStr));
                        }
                        try {
                            processUnitExecutionMapper.insertForMigration(execution);
                        } catch (DuplicateKeyException e) {
                            log.warn(execution.getId() + "-" + execution.getExecutionNo() + " insert duplicate exception");
                            processUnitExecutionMapper.updateForMigration(execution);
                        }
                        return null;
                    });
                }
            }
        };
        if (!context.isLock()) {
            runnable.run();
        } else {
            lockService.doInlock(ProcessUnitConstants.LOCK_BIZ_TYPE,
                    instance.getProcessUnitCode() + ProcessUnitConstants.LOCK_KEY_SEP + instance.getBizNo(), 10,
                    ProcessUnitConstants.DEFAULT_WAIT_SECONDS, () -> {
                        runnable.run();
                        return null;
                    });
        }
    }

    public void compare(ShardingCompareContext context) {
        for (String unitCode : context.getUnitCodeList()) {
            compareOneUnit(unitCode, context);
        }
    }

    public void compareOneUnit(String unitCode, ShardingCompareContext context) {
        ProcessUnitEntity processUnit = processUnitRepository.getProcessUnitByCode(unitCode);
        ShardGroupInfo shardGroup = shardingConfigManager.getShardGroupMap().get(unitCode);
        List<String> shardList = shardGroup.getShardList();
        for (String shard : shardList) {
            compareOneShard(unitCode, shard, shardGroup, context);
        }
    }

    private void compareOneShard(String unitCode, String shard, ShardGroupInfo shardGroup, ShardingCompareContext context) {
        log.info("Start compare unitCode:{} shard:{}", unitCode, shard);
        try {
            PagerCondition condition = new PagerCondition();
            condition.addField(new FieldEntry("processUnitCode", unitCode));
            if (context.getCreatedDateStart() != null) {
                condition.addField(new FieldEntry("createdDateStart", context.getCreatedDateStart()));
            }
            if (context.getCreatedDateEnd() != null) {
                condition.addField(new FieldEntry("createdDateEnd", context.getCreatedDateEnd()));
            }
            if (context.getInstanceNo() != null) {
                condition.addField(new FieldEntry("instanceNo", context.getInstanceNo()));
            }
            if (context.getResultList() != null) {
                condition.addField(new FieldEntry("resultList", context.getResultList()));
            }
            condition.addSortField("id", 0, "asc");
            condition.setPageSize(context.getPageSize());
            condition.setStart(-2);
            List<String> failList = new ArrayList<String>();
            shardingService.executeInShard(shard, (shardInfo) -> {
                FieldEntry idStartEntry = new FieldEntry("idStart", -1);
                condition.addField(idStartEntry);
                int count = 0;
                while (true) {
                    List<ProcessUnitInstance> list = processUnitInstanceMapper
                            .selectProcessUnitInstanceByPageCondition(condition);
                    long lastId = list.size() == 0 ? -1 : list.get(list.size() - 1).getId();
                    for (ProcessUnitInstance instance : list) {
                        compareOneInstance(instance, shardGroup, failList, context);
                        if (compareStopFlag) {
                            log.info("Compare terminal");
                            return null;
                        }
                        if (failList.size() > 1000) {
                            log.info("Compare fail list:" + failList);
                            failList.clear();
                        }
                    }
                    count += list.size();
                    log.info("shard{} compare finished count {}", shard, count);
                    if (list.size() == 0) {
                        return null;
                    }
                    idStartEntry.setValue(lastId);
                }
            });
            if (failList.size() > 0) {
                log.info("Complare fail list:" + failList);
            }
        } catch (Exception e) {
            log.error("Compare shard" + shard + " exception", e);
            throw e;
        }
    }

    private void compareOneInstance(ProcessUnitInstance instance, 
            ShardGroupInfo shardGroup, List<String> failList, ShardingCompareContext context) {
        Runnable runnable = () -> {
            String slaveShard = shardingService.computeSlaveShard(instance.getProcessUnitCode(), instance.getBizNo(),
                    shardGroup);
            if (slaveShard == null) {
                throw new EasyFlowException(instance.getProcessUnitCode() + " slaveShard not configured");
            }
            shardingService.executeInShard(slaveShard, (shardInfo) -> {
                ProcessUnitInstance newInstance = processUnitInstanceMapper.getByInstanceNo(instance.getInstanceNo());
                if (newInstance == null) {
                    log.info("{} compare fail, record in new shard not exists", instance.getInstanceNo());
                    failList.add(instance.getInstanceNo());
                    context.getFailCount().incrementAndGet();
                } else {
                    if (!Objects.equals(instance.getResult(), newInstance.getResult()) 
                            || !Objects.equals(instance.getProcessUnitCode(), newInstance.getProcessUnitCode())
                            || !Objects.equals(instance.getBizNo(), newInstance.getBizNo())
                            || !Objects.equals(instance.getRequestContent(), newInstance.getRequestContent())
                            || !Objects.equals(instance.getResponseContent(), newInstance.getResponseContent())
                            || !Objects.equals(instance.getExtData(), newInstance.getExtData())
                            || !Objects.equals(instance.getAutoRunFlag(), newInstance.getAutoRunFlag())
                            || !Objects.equals(instance.getAutoRunTimes(), newInstance.getAutoRunTimes())
                            || !Objects.equals(instance.getNextAutoRunTime(), newInstance.getNextAutoRunTime())
                            || !Objects.equals(instance.getParentNo(), newInstance.getParentNo())
                            || !Objects.equals(instance.getProductCode(), newInstance.getProductCode())) {
                        log.info("{} compare fail, old data:{} new data:{}", instance.getInstanceNo(), instance,
                                newInstance);
                        failList.add(instance.getInstanceNo());
                        context.getFailCount().incrementAndGet();
                    }
                }
                return null;
            });
        };
        if (!context.isLock()) {
            runnable.run();
        } else {
            lockService.doInlock(ProcessUnitConstants.LOCK_BIZ_TYPE,
                    instance.getProcessUnitCode() + ProcessUnitConstants.LOCK_KEY_SEP + instance.getBizNo(), 10,
                    ProcessUnitConstants.DEFAULT_WAIT_SECONDS, () -> {
                        runnable.run();
                        return null;
                    });
        }
    }

    public ProcessUnitInstanceMapper getProcessUnitInstanceMapper() {
        return processUnitInstanceMapper;
    }

    public void setProcessUnitInstanceMapper(ProcessUnitInstanceMapper processUnitInstanceMapper) {
        this.processUnitInstanceMapper = processUnitInstanceMapper;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }

    public Locker getLockService() {
        return lockService;
    }

    public void setLockService(Locker lockService) {
        this.lockService = lockService;
    }

    public boolean isMigrateStopFlag() {
        return migrateStopFlag;
    }

    public void setMigrateStopFlag(boolean migrateStopFlag) {
        this.migrateStopFlag = migrateStopFlag;
    }

    public boolean isCompareStopFlag() {
        return compareStopFlag;
    }

    public void setCompareStopFlag(boolean compareStopFlag) {
        this.compareStopFlag = compareStopFlag;
    }


    public ProcessUnitExecutionMapper getProcessUnitExecutionMapper() {
        return processUnitExecutionMapper;
    }


    public void setProcessUnitExecutionMapper(ProcessUnitExecutionMapper processUnitExecutionMapper) {
        this.processUnitExecutionMapper = processUnitExecutionMapper;
    }


    public ShardingService getShardingService() {
        return shardingService;
    }


    public void setShardingService(ShardingService shardingService) {
        this.shardingService = shardingService;
    }


    public ShardingConfigManager getShardingConfigManager() {
        return shardingConfigManager;
    }


    public void setShardingConfigManager(ShardingConfigManager shardingConfigManager) {
        this.shardingConfigManager = shardingConfigManager;
    }
    
    

    
}
