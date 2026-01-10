package com.jd.easyflow.process.infrastructure.sharding;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeExecutionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessTaskEntity;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.process.domain.repository.ProcessTaskRepository;
import com.jd.easyflow.sharding.service.ShardingData;
import com.jd.easyflow.sharding.service.ShardingDataQueryer;

/**
 * @author liyuliang5
 */
public class ProcessShardingDataQuerier extends ShardingDataQueryer {

    @Autowired
    private ProcessRepository processRepository;
    @Autowired
    private ProcessTaskRepository processTaskRepository;

    public void fillByProcessInstanceNo(String processInstanceNo, ShardingData shardingData) {
        ProcessInstanceEntity instance = shardingService.parallelExecute(shardingData.getGroup(),
                shardInfo -> processRepository.getByProcessInstanceNo(processInstanceNo),
                list -> list.stream().filter(entity -> entity != null).findFirst().orElse(null));
        if (instance != null) {
            shardingData.setGroup(instance.getProcessType());
            shardingData.setBizNo(instance.getBizNo());
        }
    }
    
    public void fillByProcessNodeInstanceNo(String processNodeInstanceNo, ShardingData shardingData) {
        ProcessNodeInstanceEntity instance = shardingService.parallelExecute(shardingData.getGroup(),
                shardInfo -> processRepository.getByNodeInstanceNo(processNodeInstanceNo),
                list -> list.stream().filter(entity -> entity != null).findFirst().orElse(null));
        if (instance != null) {
            fillByProcessInstanceNo(instance.getProcessInstanceNo(), shardingData);;
        }
    }
    
    public void fillByProcessNodeExecutionNo(String processNodeExecutionNo, ShardingData shardingData) {
        ProcessNodeExecutionEntity instance = shardingService.parallelExecute(shardingData.getGroup(),
                shardInfo -> processRepository.getByNodeExecutionNo(processNodeExecutionNo),
                list -> list.stream().filter(entity -> entity != null).findFirst().orElse(null));
        if (instance != null) {
            fillByProcessNodeInstanceNo(instance.getNodeExecutionNo(), shardingData);;
        }
    }
    
    public void fillByTaskNo(String taskNo, ShardingData shardingData) {
        ProcessTaskEntity task = shardingService.parallelExecute(shardingData.getGroup(),
                shardInfo -> processTaskRepository.getTask(taskNo),
                list -> list.stream().filter(entity -> entity != null).findFirst().orElse(null));
        if (task != null) {
            shardingData.setGroup(task.getProcessType());
            shardingData.setBizNo(task.getBizNo());
        }
    }
    

    public ProcessRepository getProcessRepository() {
        return processRepository;
    }

    public void setProcessRepository(ProcessRepository processRepository) {
        this.processRepository = processRepository;
    }

}
