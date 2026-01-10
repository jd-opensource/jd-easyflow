package com.jd.easyflow.process.adapter.export.sharding;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.process.adapter.export.ProcessToolExportImpl;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.RollbackNodeReq;
import com.jd.easyflow.process.adapter.export.dto.instance.RollbackNodeRes;
import com.jd.easyflow.process.infrastructure.sharding.ProcessShardingDataQuerier;
import com.jd.easyflow.sharding.service.ExportRequestShardComputer;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 * @author liyuliang5
 */
public class ProcessToolExportShardingImpl extends ProcessToolExportImpl {
    
    private ProcessShardingDataQuerier shardingDataQuerier;
    
    private ShardingService shardingService;
    
    private ExportRequestShardComputer shardComputer;
    
    @Action(code = "easyflow-process-rollbackNode", name = "rollbackNode")
    @Override
    public ExportResponse<RollbackNodeRes> rollbackNode(ExportRequest<RollbackNodeReq> request) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(request,  shardingData->{
                String processInstanceNo = request.getData().getProcessInstanceNo();
                AssertUtils.isNotNull(processInstanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(processInstanceNo, shardingData);
            });
        }, shard -> {
            return super.rollbackNode(request);
        });
    }
    
    @Action(code = "easyflow-process-updateProcessInstance", name = "updateProcessInstance")
    @Override
    public ExportResponse updateProcessInstance(ExportRequest<ProcessInstanceDTO> request) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(request,  shardingData->{
                String processInstanceNo = request.getData().getInstanceNo();
                AssertUtils.isNotNull(processInstanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(processInstanceNo, shardingData);
            });
        }, shard -> {
            return super.updateProcessInstance(request);
        });
    }


    @Action(code = "easyflow-process-updateProcessNodeInstance", name = "updateProcessNodeInstance")
    @Override
    public ExportResponse updateProcessNodeInstance(ExportRequest<ProcessNodeInstanceDTO> request) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(request,  shardingData->{
                String nodeInstanceNo = request.getData().getNodeInstanceNo();
                AssertUtils.isNotNull(nodeInstanceNo);
                shardingDataQuerier.fillByProcessNodeInstanceNo(nodeInstanceNo, shardingData);
            });
        }, shard -> {
            return super.updateProcessNodeInstance(request);
        });
    }

    @Action(code = "easyflow-process-deleteProcessNodeInstance", name = "deleteProcessNodeInstance")
    @Override
    public ExportResponse deleteProcessNodeInstance(ExportRequest<String> request) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(request,  shardingData->{
                String nodeInstanceNo = request.getData();
                AssertUtils.isNotNull(nodeInstanceNo);
                shardingDataQuerier.fillByProcessNodeInstanceNo(nodeInstanceNo, shardingData);
            });
        }, shard -> {
            return super.deleteProcessNodeInstance(request);
        });
    }

    public ProcessShardingDataQuerier getShardingDataQuerier() {
        return shardingDataQuerier;
    }

    public void setShardingDataQuerier(ProcessShardingDataQuerier shardingDataQuerier) {
        this.shardingDataQuerier = shardingDataQuerier;
    }


    public ExportRequestShardComputer getShardComputer() {
        return shardComputer;
    }

    public void setShardComputer(ExportRequestShardComputer shardComputer) {
        this.shardComputer = shardComputer;
    }

    public ShardingService getShardingService() {
        return shardingService;
    }

    public void setShardingService(ShardingService shardingService) {
        this.shardingService = shardingService;
    }
    
    
    
    

}
