package com.jd.easyflow.sharding.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.sharding.CurrentShardInfo;
import com.jd.easyflow.sharding.ShardingHolder;
import com.jd.easyflow.sharding.config.ShardGroupInfo;
import com.jd.easyflow.sharding.config.ShardInfo;
import com.jd.easyflow.sharding.config.ShardingConfigManager;
import com.jd.easyflow.sharding.service.ShardingComputeResult;
import com.jd.easyflow.sharding.service.ShardingData;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 * @author liyuliang5
 */
public class ShardingServiceImpl implements ShardingService  {

    private static final Logger logger = LoggerFactory.getLogger(ShardingServiceImpl.class);
    
    protected boolean shardingEnabled;
    
    protected boolean parallelQuery = true;
    
    protected int parallelQueryMaxThreadCount = 10;
    
    protected ExecutorService parallelQueryThreadPool = Executors.newCachedThreadPool();
    
    protected int parallelTimeoutMillis = 30000;
    
    protected ShardingConfigManager shardingConfigManager;

    public <T> T execute(int opType, String group, String bizNo, Function<CurrentShardInfo, T> function) {
        return execute(opType, group, bizNo, false, function);
    }
    public <T> T execute(int opType, String group, String bizNo, boolean usingSlaveDb, Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        AssertUtils.isNotNull(group);
        AssertUtils.isNotNull(bizNo);
        ShardGroupInfo shardGroup = getGroupShardConfig(group);
        // master db
        String shard = computeShard(group, bizNo, shardGroup);
        if (shard == null) {
            throw new EasyFlowException("group:" + group + " bizNo:" + bizNo + " shard is null");
        }
        T result = executeInShard(shard, usingSlaveDb, function);

        // slave db on double write
        if (OP_TYPE_WRITE == opType) {
            boolean doubleWrite = Boolean.TRUE.equals(shardGroup.getDoubleWrite());
            if (doubleWrite) {
                boolean ignoreSlaveError = Boolean.TRUE
                        .equals(shardGroup.getIgnoreSlaveError());
                String slaveShard = computeSlaveShard(group, bizNo, shardGroup);
                if (slaveShard == null) {
                    logger.warn("group:{} bizNo:{} slave shard not config", group, bizNo);
                } else {
                    try {
                        executeInShard(slaveShard, usingSlaveDb, function);
                    } catch (Throwable t) {
                        logger.error("group:" + group + " bizNo:" + bizNo + " write slave exception", t);
                        if (!ignoreSlaveError) {
                            throw t;
                        }
                    }
                }
            }
        }
        return result;
    }
    
    // == get One ==
    
    public <T> T getOneInShardsOfGroup(String group, Function<CurrentShardInfo, T> function) {
        return getOneInShardsOfGroup(group, false, function);
    }
    
    public <T> T getOneInShardsOfGroup(String group, boolean usingSlaveDb, Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        List<String> shardList = this.getShardListOfGroup(group);
        return parallelExecute(shardList, usingSlaveDb, shardInfo -> {
            T t = executeInShard(shardInfo, function);
            if (t != null) {
                if (logger.isDebugEnabled()) {
                    logger.debug("found result in shard {}", shardInfo);
                }
            }
            return t;
        }, shardingResultList -> {
            T result = null;
            for (T shardResult : shardingResultList) {
                if (shardResult != null) {
                    if (result != null) {
                        throw new EasyFlowException("result count more than 2," + shardResult + "," + result);
                    } else {
                        result = shardResult;
                    }
                }
            }
            return result;
        });
    }
    
    public <T> T  getOneInAllShards(Function<CurrentShardInfo, T> function) {
        return getOneInAllShards(false, function);
    }
    
    public <T> T getOneInAllShards(boolean usingSlaveDb, Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        Collection<ShardInfo> shardList = shardingConfigManager.getShardMap().values();
        Collection<ShardInfo> masterShardList = filterMasterShard(shardList);
        return parallelExecuteInShardList(masterShardList, usingSlaveDb, shardingInfo -> {
            T t = executeInShard(shardingInfo, function);
            if (t != null) {
                if (logger.isDebugEnabled()) {
                    logger.debug("found result in shard {}", shardingInfo);
                }
            }
            return t;
        }, shardingResultList -> {
            T result = null;
            for (T shardResult : shardingResultList) {
                if (shardResult != null) {
                    if (result != null) {
                        throw new EasyFlowException("result count more than 2," + shardResult + "," + result);
                    } else {
                        result = shardResult;
                    }
                }
            }
            return result;
        });
    }

  // == find list ==
    
    public List findListInAllShards(Function<CurrentShardInfo, List> function) {
        return findListInAllShards(false, function);
    }
    
    public List findListInAllShards(boolean usingSlaveDb, Function<CurrentShardInfo, List> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        Collection<ShardInfo> shardList = shardingConfigManager.getShardList();
        Collection<ShardInfo> masterShardList = filterMasterShard(shardList);
        return parallelExecuteInShardList(masterShardList,usingSlaveDb, shardingInfo -> {
            List listOfShard = executeInShard(shardingInfo, function);
            return listOfShard;
        }, shardingResultList -> {
            List result = new ArrayList<>();
            for (List shardResult : shardingResultList) {
                result.addAll(shardResult);
            }
            return result;
        });

    }
    

    public List findListInShardsOfGroup(String group, boolean usingSlaveDb,
            Function<CurrentShardInfo, List> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        List<String> shardList = getShardListOfGroup(group);
        return parallelExecute(shardList, usingSlaveDb, shardingInfo -> {
            List listOfShard = executeInShard(shardingInfo, function);
            return listOfShard;
        }, shardResultList -> {
            List result = new ArrayList<>();
            for (List shardResult : shardResultList) {
                result.addAll(shardResult);
            }
            return result;
        });
    }
    
    public List findListLoopShardsOfGroup(String group, int maxCount, boolean usingSlaveDb, Function<CurrentShardInfo, List> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        List<String> shardList = getShardListOfGroup(group);
        Map<String, ShardInfo> shardingInfoMap = shardingConfigManager.getShardMap();
        List result = new ArrayList();
        for (String shard : shardList) {
            ShardInfo info = shardingInfoMap.get(shard);
            if (! info.isMaster()) {
                continue;
            }
                CurrentShardInfo currentShardingInfo = buildCurrentShardInfo(info, usingSlaveDb);
                List list = executeInShard(currentShardingInfo, function);
                if (list != null && list.size() > 0) {
                    result.addAll(list);
                    if (result.size() >= maxCount) {
                        return result;
                    }
                }
        }
        return result;
    }
    
    public List findListLoopAllShards(int maxCount, Function<CurrentShardInfo, List> function) {
        return findListLoopAllShards(maxCount, false, function);
    }
    
    public List findListLoopAllShards(int maxCount, boolean usingSlaveDb, Function<CurrentShardInfo, List> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        List result = new ArrayList();
        for (ShardInfo info : shardingConfigManager.getShardMap().values()) {
            if (!info.isMaster()) {
                continue;
            }
            CurrentShardInfo currentShardInfo = buildCurrentShardInfo(info, usingSlaveDb);
            List list = executeInShard(currentShardInfo, function);
            if (list != null && list.size() > 0) {
                result.addAll(list);
                if (result.size() >= maxCount) {
                    return result;
                }
            }
        }
        return result;
    }
    
    // == count ==
    
    public long countInAllShards(boolean usingSlaveDb, Function<CurrentShardInfo, Long> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        Collection<ShardInfo> shardList = shardingConfigManager.getShardList();
        Collection<ShardInfo> masterShardList = filterMasterShard(shardList);
        return parallelExecuteInShardList(masterShardList, usingSlaveDb, shardingInfo -> {
            Long countOfShard = executeInShard(shardingInfo, function);
            return countOfShard;
        }, shardingResultList -> {
            long count = 0;
            for (Long countInShard : shardingResultList) {
                count += countInShard;
            }
            return count;
        });
    }
    
    
    
    public long countInShardsOfGroup(String group, boolean usingSlaveDb,
            Function<CurrentShardInfo, Long> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        List<String> shardList = getShardListOfGroup(group);
        return parallelExecute(shardList, usingSlaveDb, shardingInfo -> {
            Long countInShard = executeInShard(shardingInfo, function);
            return countInShard;
        }, shardResultList -> {
            long count = 0;
            for (Long countInShard : shardResultList) {
                count += countInShard;
            }
            return count;
        });
    }
    
    // == loop execute ==
    
    public <R, SR> R loopExecuteInAllShards(Function<CurrentShardInfo, SR> function, Function<List<SR>, R> resultCollector) {
        if (! shardingEnabled) {
            return resultCollector.apply(Arrays.asList(function.apply(null)));
        }
        Collection<ShardInfo> shardingList = shardingConfigManager.getShardList();
        return loopExecuteInShardList(shardingList, shardInfo -> {
            CurrentShardInfo currentShardInfo = buildCurrentShardInfo(shardInfo, false);
            SR shardingResult = function.apply(currentShardInfo);
            return shardingResult;
        }, resultCollector);
    }
    
    private <R, SR> R loopExecuteInShardList(Collection<ShardInfo> shardInfoList,
            Function<ShardInfo, SR> shardFunction, Function<List<SR>, R> resultCollector) {
        if (! shardingEnabled) {
            return resultCollector.apply(Arrays.asList(shardFunction.apply(null)));
        }
        List<SR> resultList = new ArrayList<SR>();
        for (ShardInfo info : shardInfoList) {
            SR shardResult = shardFunction.apply(info);
            resultList.add(shardResult);
        }
        R result = resultCollector.apply(resultList);
        return result;
    }

    // == parallel execute ==
    
    public <R, SR> R parallelExecute(String group, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector) {
        return this.parallelExecute(group, false, shardFunction, resultCollector);
    }
    
    public <R, SR> R parallelExecute(String group, boolean usingSlaveDb, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector) {
        if (! shardingEnabled) {
            return resultCollector.apply(Arrays.asList(shardFunction.apply(null)));
        }
        if (group == null) {
            return parallelExecuteInAllShard(usingSlaveDb, shardFunction, resultCollector);
        } else {
            return parallelExecuteInGroup(group, usingSlaveDb, shardFunction, resultCollector);
        }
    }
    
    
    public <R, SR> R parallelExecuteInGroup(String group, boolean usingSlaveDb, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector) {
        if (! shardingEnabled) {
            return resultCollector.apply(Arrays.asList(shardFunction.apply(null)));
        }
            List<String> shardList = shardingConfigManager.getShardGroupMap().get(group).getShardList();
            return parallelExecute(shardList, usingSlaveDb, shardFunction, resultCollector);
    }
    
    public <R, SR> R parallelExecuteInAllShard(boolean usingSlaveDb, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector) {
        if (! shardingEnabled) {
            return resultCollector.apply(Arrays.asList(shardFunction.apply(null)));
        }
            List<ShardInfo> shardList = shardingConfigManager.getShardList();
            return parallelExecuteInShardList(shardList, usingSlaveDb, shardFunction, resultCollector);
    }
    
    private <R, SR> R parallelExecute(List<String> shardList, boolean usingSlaveDb, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector) {
        if (! shardingEnabled) {
            return resultCollector.apply(Arrays.asList(shardFunction.apply(null)));
        }
        R result = null;
        List<SR> shardResultList = new ArrayList<SR>();
        Map<String, ShardInfo> shardingInfoMap = shardingConfigManager.getShardMap();
        if (!parallelQuery || shardList.size() <= 1) {
            for (String shard : shardList) {
                ShardInfo shardingInfo = shardingInfoMap.get(shard);
                
                SR shardResult = executeInShard(shardingInfo, usingSlaveDb, currentShardingInfo -> shardFunction.apply(currentShardingInfo));
                shardResultList.add(shardResult);
            }
        } else {
            int executedShardCount = 0;
            int totalShardCount = shardList.size();
            while (executedShardCount < totalShardCount) {
                List<CompletableFuture<SR>> futureList = new ArrayList<CompletableFuture<SR>>();
                for (int i = 0; i < parallelQueryMaxThreadCount; i++) {
                    ShardInfo shardingInfo = shardingConfigManager.getShardMap().get(shardList.get(executedShardCount));
                    CompletableFuture<SR> future = CompletableFuture.supplyAsync(() -> {
                        SR shardResult = executeInShard(shardingInfo, usingSlaveDb, currentShardInfo -> {
                            return shardFunction.apply(currentShardInfo);
                        });
                        return shardResult;
                    }, parallelQueryThreadPool);
                    futureList.add(future);
                    executedShardCount++;
                    if (executedShardCount == totalShardCount) {
                        break;
                    }
                }
                for (CompletableFuture<SR> future : futureList) {
                    try {
                        SR shardResult = future.get(parallelTimeoutMillis, TimeUnit.MILLISECONDS);
                        shardResultList.add(shardResult);
                    } catch (InterruptedException | ExecutionException | TimeoutException e) {
                        throw new EasyFlowException("multiple thread shard query exception", e);
                    }
                }
            }
        }
        result = resultCollector.apply(shardResultList);

        return result;
    }
    
    private <R, SR> R parallelExecuteInShardList(Collection<ShardInfo> shardList, boolean usingSlaveDb,
            Function<CurrentShardInfo, SR> shardFunction, Function<List<SR>, R> resultCollector) {
        if (! shardingEnabled) {
            return resultCollector.apply(Arrays.asList(shardFunction.apply(null)));
        }
        R result = null;
        List<SR> shardResultList = new ArrayList<SR>();
        if (!parallelQuery || shardList.size() <= 1) {
            for (ShardInfo shard : shardList) {
                SR shardResult = executeInShard(shard, usingSlaveDb, currentShardingInfo -> shardFunction.apply(currentShardingInfo));
                shardResultList.add(shardResult);
            }
        } else {
            int executedShardCount = 0;
            Iterator<ShardInfo> iterator = shardList.iterator();
            int totalShardCount = shardList.size();
            while (executedShardCount < totalShardCount) {
                List<CompletableFuture<SR>> futureList = new ArrayList<CompletableFuture<SR>>();
                for (int i = 0; i < parallelQueryMaxThreadCount; i++) {
                    ShardInfo shardingInfo = iterator.next();
                    CompletableFuture<SR> future = CompletableFuture.supplyAsync(() -> {
                        SR shardResult = executeInShard(shardingInfo, usingSlaveDb, currentShardInfo -> {
                            return shardFunction.apply(currentShardInfo);
                        });
                        return shardResult;
                    }, parallelQueryThreadPool);
                    futureList.add(future);
                    executedShardCount++;
                    if (executedShardCount == totalShardCount) {
                        break;
                    }
                }
                for (CompletableFuture<SR> future : futureList) {
                    try {
                        SR shardResult = future.get(parallelTimeoutMillis, TimeUnit.MILLISECONDS);
                        shardResultList.add(shardResult);
                    } catch (InterruptedException | ExecutionException | TimeoutException e) {
                        throw new EasyFlowException("multiple thread shard query exception", e);
                    }
                }
            }
        }
        result = resultCollector.apply(shardResultList);

        return result;
    }
    
    
    
    // == execute in shard ==
    public <T> T executeInDefaultShard(Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        ShardGroupInfo shardGroup = shardingConfigManager.getDefaultShardGroup();
        if (shardGroup == null) {
            throw new EasyFlowException("default shard group is null");
        }        
        if (shardGroup.getShardList().size() != 1) {
            throw new EasyFlowException("default shard group size is not 1");
        }
        ShardInfo info = shardingConfigManager.getShardMap().get(shardGroup.getShardList().get(0));
        if (info == null) {
            throw new EasyFlowException("default shard:" + shardGroup.getShardList().get(0) + " shard info is null");
        }
        return executeInShard(info, false, function);
    }
    
    @Override
    public <T> T executeInShard(String shard, Function<CurrentShardInfo, T> function) {
        return executeInShard(shard, false, function);
    }
    public <T> T executeInShard(String shard, boolean usingSlaveDb, Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }       
        ShardInfo info = shardingConfigManager.getShardMap().get(shard);
        if (info == null) {
            throw new EasyFlowException("shard:" + shard+ " shard info is null");
        }
        return executeInShard(info, usingSlaveDb, function);
    }
    
    public <T> T executeInShard(ShardInfo info, boolean usingSlaveDb, Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        if (info == null) {
            throw new EasyFlowException(" shard info is null");
        }
        CurrentShardInfo old = ShardingHolder.getCurrentShardInfo();
        try {
            CurrentShardInfo currentShardingInfo = buildCurrentShardInfo(info, usingSlaveDb);
            ShardingHolder.setCurrentShardInfo(currentShardingInfo);
            T result = function.apply(currentShardingInfo);
            return result;
        } finally {
            ShardingHolder.setCurrentShardInfo(old);
        }
    }
    
    public <T>T executeInShard(Supplier<CurrentShardInfo> shardComputer, Function<CurrentShardInfo, T> function) {
        return executeInShard(shardComputer, null, function);
    }
    
    public <T>T executeInShard(Supplier<CurrentShardInfo> shardComputer, Supplier<T> noShardFunction, Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        CurrentShardInfo info = shardComputer.get();
        if (info == null) {
            if (noShardFunction == null) {
                throw new EasyFlowException(" shard info is null");
            } else {
                return noShardFunction.get();
            }
        }
        CurrentShardInfo old = ShardingHolder.getCurrentShardInfo();
        try {
            ShardingHolder.setCurrentShardInfo(info);
            return function.apply(info);
        } finally {
            ShardingHolder.setCurrentShardInfo(old);
        }
    }
    
    
    public <T> T executeInShard(CurrentShardInfo currentShardingInfo, Function<CurrentShardInfo, T> function) {
        if (! shardingEnabled) {
            return function.apply(null);
        }
        if (currentShardingInfo == null) {
            throw new EasyFlowException(" shard info is null");
        }
        CurrentShardInfo old = ShardingHolder.getCurrentShardInfo();
        try {
            ShardingHolder.setCurrentShardInfo(currentShardingInfo);
            T result = function.apply(currentShardingInfo);
            return result;
        } finally {
            ShardingHolder.setCurrentShardInfo(old);
        }
    }
    
    
    //== shard compute ==
    
    public String computeShardOfGroupAndBizNo(String group, String bizNo) {
        if (! shardingEnabled) {
            return null;
        }
        ShardGroupInfo shardGroupInfo = getGroupShardConfig(group);
        String shard = computeShard(group, bizNo, shardGroupInfo);
        return shard;
    }
    
    public ShardingComputeResult computeShardInfo(String group, String bizNo) {
        if (! shardingEnabled) {
            return null;
        }
        ShardingComputeResult shardingInfo = new ShardingComputeResult();
        ShardGroupInfo shardGroup = shardingConfigManager.getShardGroupMap().get(group);
        String shard = computeShard(group, bizNo, shardGroup);
        ShardInfo info = shardingConfigManager.getShardMap().get(shard);
        if (info != null) {
            shardingInfo.setDb(info.getDb());
            shardingInfo.setTableSuffix(info.getTableSuffix());
            shardingInfo.setShard(shard);
        }
        String slaveShard = computeSlaveShard(group, bizNo, shardGroup);
        ShardInfo slaveInfo = shardingConfigManager.getShardMap().get(slaveShard);
        if (slaveInfo != null) {
            shardingInfo.setSlaveShard(slaveShard);
            shardingInfo.setSlaveDb(slaveInfo.getDb());
            shardingInfo.setSlaveTableSuffix(slaveInfo.getTableSuffix());
        }
        return shardingInfo;
    }
    
    
    private String computeShardByHashMode(String group, String bizNo, List<String> shardList) {
        if (shardList == null || shardList.size() == 0) {
            return null;
        }
        int hash = Math.abs(bizNo.hashCode());
        int shardIndex = hash % shardList.size();
        String shard = shardList.get(shardIndex);
        return shard;
    }
    
    public String getShard(ShardingData shardingData) {
        if (! shardingEnabled) {
            return null;
        }
        String shard = null;
        if (shardingData.getShard() != null) {
            shard = shardingData.getShard();
        } else if (shardingData.getGroup() != null && shardingData.getBizNo() != null) {
            shard = computeShardOfGroupAndBizNo(shardingData.getGroup(), shardingData.getBizNo());
        } else if (shardingData.getGroup() != null) {
            List<String> shardList = getShardListOfGroup(shardingData.getGroup());
            if (shardList.size() == 1) {
                shard = shardList.get(0);
            }
        }
        return shard;
    }

    
    
    private String computeShard(String group, String bizNo, ShardGroupInfo shardGroup) {
        List<String> shardList = shardGroup.getShardList();
        String shard = null;
        shard = computeShardByHashMode(group, bizNo, shardList);
        if (logger.isDebugEnabled()) {
            logger.debug("group:{} bizNo:{} use shard:{}", group, bizNo, shard);
        }
        return shard;
    }
    
    public String computeSlaveShard(String group, String bizNo, ShardGroupInfo shardGroup) {
        if (! shardingEnabled) {
            return null;
        }
        List<String> shardList = shardGroup.getSlaveShardList();
        String shard = null;
            shard = computeShardByHashMode(group, bizNo, shardList);
            if (logger.isDebugEnabled()) {
                logger.debug("group:{} bizNo:{} use slave shard:{}", group, bizNo, shard);
            }
        return shard;
    }
    
    
    // == shard filter ==
    
    private Collection<ShardInfo> filterMasterShard(Collection<ShardInfo> shardList) {
        if (! shardingEnabled) {
            return null;
        }
        boolean containsSlave = false;
        for (ShardInfo info : shardList) {
            if (! info.isMaster()) {
                containsSlave = true;
                break;
            }
        }
        if (! containsSlave) {
            return shardList;
        }
        List<ShardInfo> result = new ArrayList<ShardInfo>(shardList.size());
        for (ShardInfo info : shardList) {
            if (info.isMaster()) {
                result.add(info);
            }
        }
        return result;
    }
    
    private Collection<ShardInfo> filterSlaveShard(Collection<ShardInfo> shardList) {
        List<ShardInfo> result = new ArrayList<ShardInfo>(shardList.size());
        for (ShardInfo info : shardList) {
            if (! info.isMaster()) {
                result.add(info);
            }
        }
        return result;
    }
    
    @Override
    public ShardGroupInfo getGroupShardConfig(String group) {
        if (! shardingEnabled) {
            return null;
        }
        if (shardingConfigManager.isFixDefaultShard()) {
            return shardingConfigManager.getDefaultShardGroup();
        }
        ShardGroupInfo shardGroupInfo = shardingConfigManager.getShardGroupMap().get(group);
        if (shardGroupInfo != null) {
            return shardGroupInfo;
        }
        return shardingConfigManager.getDefaultShardGroup();
    }
    
    public List<String> getShardListOfGroup(String group) {
        if (! shardingEnabled) {
            return null;
        }
        ShardGroupInfo processShardConfig = getGroupShardConfig(group);
        return processShardConfig.getShardList();
    }
    
   
    
    public CurrentShardInfo buildCurrentShardInfo(String shard, boolean usingSlaveDb) {
        if (! shardingEnabled) {
            return null;
        }
        ShardInfo info = shardingConfigManager.getShardMap().get(shard);
        return buildCurrentShardInfo(info, usingSlaveDb);
    }
    

    private CurrentShardInfo buildCurrentShardInfo(ShardInfo shardInfo, boolean usingSlaveDb) {
        CurrentShardInfo info = new CurrentShardInfo();
        info.setTableSuffix(shardInfo.getTableSuffix());
        info.setShard(shardInfo.getShard());
        if (! usingSlaveDb) {
            info.setDb(shardInfo.getDb());
        } else {
            String slaveDb = shardInfo.getSlaveDb();
            if (slaveDb == null) {
                if (logger.isDebugEnabled()) {
                    logger.debug("slaveDb is null, use master db " + shardInfo.getDb());
                }
                info.setDb(shardInfo.getDb());
            } else {
                info.setDb(slaveDb);
            }
        }
        return info;
    }
    
    
    
    
    
    

    public boolean isParallelQuery() {
        return parallelQuery;
    }

    public void setParallelQuery(boolean parallelQuery) {
        this.parallelQuery = parallelQuery;
    }

    public int getParallelQueryMaxThreadCount() {
        return parallelQueryMaxThreadCount;
    }

    public void setParallelQueryMaxThreadCount(int parallelQueryMaxThreadCount) {
        this.parallelQueryMaxThreadCount = parallelQueryMaxThreadCount;
    }

    public ExecutorService getParallelQueryThreadPool() {
        return parallelQueryThreadPool;
    }

    public void setParallelQueryThreadPool(ExecutorService parallelQueryThreadPool) {
        this.parallelQueryThreadPool = parallelQueryThreadPool;
    }

    public int getParallelTimeoutMillis() {
        return parallelTimeoutMillis;
    }

    public void setParallelTimeoutMillis(int parallelTimeoutMillis) {
        this.parallelTimeoutMillis = parallelTimeoutMillis;
    }

    public ShardingConfigManager getShardingConfigManager() {
        return shardingConfigManager;
    }

    public void setShardingConfigManager(ShardingConfigManager shardingConfigManager) {
        this.shardingConfigManager = shardingConfigManager;
    }
    public boolean isShardingEnabled() {
        return shardingEnabled;
    }
    public void setShardingEnabled(boolean shardingEnabled) {
        this.shardingEnabled = shardingEnabled;
    }
    
}
