package com.jd.easyflow.sharding.service;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import com.jd.easyflow.sharding.CurrentShardInfo;
import com.jd.easyflow.sharding.config.ShardGroupInfo;
import com.jd.easyflow.sharding.config.ShardInfo;

/**
 * @author liyuliang5
 */
public interface ShardingService {
    
    public static final int OP_TYPE_READ = 1;
    public static final int OP_TYPE_WRITE = 2;
    
    public <T> T execute(int opType, String group, String bizNo, Function<CurrentShardInfo, T> function);
    public <T> T execute(int opType, String group, String bizNo, boolean usingSlaveDb, Function<CurrentShardInfo, T> function);
    
    // == get One ==
    
    public <T> T getOneInShardsOfGroup(String group, Function<CurrentShardInfo, T> function);
    
    public <T> T getOneInShardsOfGroup(String group, boolean usingSlaveDb, Function<CurrentShardInfo, T> function);
    
    public <T> T  getOneInAllShards(Function<CurrentShardInfo, T> function);
    
    public <T> T getOneInAllShards(boolean usingSlaveDb, Function<CurrentShardInfo, T> function);

  // == find list ==
    
    public List findListInAllShards(Function<CurrentShardInfo, List> function);
    
    public List findListInAllShards(boolean usingSlaveDb, Function<CurrentShardInfo, List> function);
    
    public List findListInShardsOfGroup(String group, boolean usingSlaveDb,  Function<CurrentShardInfo, List> function);
    
    public List findListLoopShardsOfGroup(String group, int maxCount, boolean usingSlaveDb, Function<CurrentShardInfo, List> function);
    
    public List findListLoopAllShards(int maxCount, Function<CurrentShardInfo, List> function);
    
    public List findListLoopAllShards(int maxCount, boolean usingSlaveDb, Function<CurrentShardInfo, List> function);
    
    // == count ==
    
    public long countInAllShards(boolean usingSlaveDb, Function<CurrentShardInfo, Long> function);
    
    public long countInShardsOfGroup(String group, boolean usingSlaveDb, Function<CurrentShardInfo, Long> function);
    
    // == loop execute ==
    
    public <R, SR> R loopExecuteInAllShards(Function<CurrentShardInfo, SR> function, Function<List<SR>, R> resultCollector);
    
    // == parallel execute ==
    public <R, SR> R parallelExecute(String group, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector);
    
    public <R, SR> R parallelExecute(String group, boolean usingSlaveDb, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector);
    
    
    public <R, SR> R parallelExecuteInGroup(String group, boolean usingSlaveDb, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector) ;
    
    public <R, SR> R parallelExecuteInAllShard(boolean usingSlaveDb, Function<CurrentShardInfo, SR> shardFunction,
            Function<List<SR>, R> resultCollector);
    
    
    // == execute in shard ==
    public <T> T executeInDefaultShard(Function<CurrentShardInfo, T> function);
    
    public <T> T executeInShard(String shard, Function<CurrentShardInfo, T> function);
    
    public <T> T executeInShard(String shard, boolean usingSlaveDb, Function<CurrentShardInfo, T> function);
    
    public <T> T executeInShard(ShardInfo info, boolean usingSlaveDb, Function<CurrentShardInfo, T> function);
    
    public <T>T executeInShard(Supplier<CurrentShardInfo> shardComputer, Function<CurrentShardInfo, T> function);
    
    public <T>T executeInShard(Supplier<CurrentShardInfo> shardComputer, Supplier<T> noShardFunction, Function<CurrentShardInfo, T> function);

    
    public <T> T executeInShard(CurrentShardInfo currentShardingInfo, Function<CurrentShardInfo, T> function);
    
    //== shard compute ==
    
    public String computeShardOfGroupAndBizNo(String group, String bizNo);
    
    public ShardingComputeResult computeShardInfo(String group, String bizNo);
    
    public String getShard(ShardingData shardingData);
    
    public String computeSlaveShard(String group, String bizNo, ShardGroupInfo shardGroup);
 
    public List<String> getShardListOfGroup(String group);
   
    public CurrentShardInfo buildCurrentShardInfo(String shard, boolean usingSlaveDb);
    
    // == other ==
    public ShardGroupInfo getGroupShardConfig(String group);
    
    public boolean isShardingEnabled();
}
