package com.jd.easyflow.process.adapter.export.sharding;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.process.adapter.export.ProcessTaskExportImpl;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskRes;
import com.jd.easyflow.process.adapter.export.dto.task.ExecuteTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.ExecuteTaskRes;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.QueryTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperationsReq;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperationsRes;
import com.jd.easyflow.process.adapter.export.dto.task.WithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.WithdrawTaskRes;
import com.jd.easyflow.process.infrastructure.sharding.ProcessShardingDataQuerier;
import com.jd.easyflow.sharding.CurrentShardInfo;
import com.jd.easyflow.sharding.service.ExportRequestShardComputer;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 * @author liyuliang5
 */
public class ProcessTaskExportShardingImpl extends ProcessTaskExportImpl {
    
    
    private ProcessShardingDataQuerier shardingDataQuerier;
    
    private ShardingService shardingService;
    
    private ExportRequestShardComputer shardComputer;
    
    @Action(code = "easyflow-process-executeTask", name = "executeTask")
    @Override
    public ExportResponse<ExecuteTaskRes> executeTask(ExportRequest<ExecuteTaskReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String taskNo = req.getData().getTaskNo();
                AssertUtils.isNotNull(taskNo);
                shardingDataQuerier.fillByTaskNo(taskNo, shardingData);
            });
        }, shard -> {
            return super.executeTask(req);
        });
    }

    @Action(code = "easyflow-process-pagerQueryTask", name = "pagerQueryTask")
    @Override
    public ExportResponse<PagerResult> pagerQueryTask(ExportRequest<PagerCondition> req) {
        if (! shardingService.isShardingEnabled()) {
            return super.pagerQueryTask(req);
        }
        CurrentShardInfo shardInfo = shardComputer.computeExportRequestShard(req,  shardingData->{
            PagerCondition condition = req.getData();
            String processType = condition.getField("processType") == null ? null : (String) condition.getField("processType").getValue();
            String bizNo = condition.getField("bizNo") == null ? null : (String) condition.getField("bizNo").getValue();
            String processInstanceNo = condition.getField("processInstanceNo") == null ? null : (String) condition.getField("processInstanceNo").getValue();
            String nodeInstanceNo = condition.getField("nodeInstanceNo") == null ? null : (String) condition.getField("nodeInstanceNo").getValue();
            if (processType != null && bizNo != null) {
                shardingData.setGroup(processType);
                shardingData.setBizNo(bizNo);
            } else if (processInstanceNo != null) {
                shardingDataQuerier.fillByProcessInstanceNo(processInstanceNo, shardingData);
            } else if (nodeInstanceNo != null) {
                shardingDataQuerier.fillByProcessNodeInstanceNo(nodeInstanceNo, shardingData);
            } else if (processType != null) {
               if (shardingService.getShardListOfGroup(processType).size() == 1) {
                   shardingData.setGroup(processType);
               }
            } 
        });
        if (shardInfo != null ) {
            return shardingService.executeInShard(shardInfo, shard -> {
                return super.pagerQueryTask(req);
            });
        } else {
            req.getData().addSortField("taskNo", 0, "desc");
            return shardingService.parallelExecuteInAllShard(false, info -> {
                return super.pagerQueryTask(req);
            }, subList -> {
                long count = 0;
                List<ProcessTaskDTO> result = new ArrayList<>();
                for (ExportResponse<PagerResult> res : subList) {
                    PagerResult pagerResult = ExportResponseUtil.unwrap(res);
                    count += pagerResult.getCount() == null ? 0 : pagerResult.getCount();
                    result.addAll(pagerResult.getList());
                }
                Collections.sort(result, (task1, task2) -> {
                    return -task1.getTaskNo().compareTo(task2.getTaskNo());
                });
                PagerResult pagerResult = new PagerResult<>();
                pagerResult.setCount(count);
                if (result.size() > req.getData().getPageSize()) {
                    result = result.subList(0, req.getData().getPageSize());
                }
                pagerResult.setList(result);

                return ExportResponse.build4Success(pagerResult);
            });
        }
    }

    @Action(code = "easyflow-process-getTask", name = "getTask")
    @Override
    public ExportResponse<ProcessTaskDTO> getTask(ExportRequest<String> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String taskNo = req.getData();
                AssertUtils.isNotNull(taskNo);
                shardingDataQuerier.fillByTaskNo(taskNo, shardingData);
            });
        }, ()-> {
            return ExportResponse.build4Success();
        }, shard -> {
            return super.getTask(req);
        });
    }

    @Action(code = "easyflow-process-queryTask", name = "queryTask")
    @Override
    public ExportResponse<List<ProcessTaskDTO>> queryTask(ExportRequest<QueryTaskReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                QueryTaskReq query = req.getData();
                if (query.getProcessType() != null && query.getBizNo() != null) {
                    shardingData.setGroup(query.getProcessType());
                    shardingData.setBizNo(query.getBizNo());
                } else if (query.getProcessInstanceNo() != null) {
                    shardingDataQuerier.fillByProcessInstanceNo(query.getProcessInstanceNo(), shardingData);
                } else if (query.getNodeInstanceNo() != null) {
                    shardingDataQuerier.fillByProcessNodeInstanceNo(query.getNodeInstanceNo(), shardingData);
                } else if (query.getProcessType() != null) {
                   if (shardingService.getShardListOfGroup(query.getProcessType()).size() == 1) {
                       shardingData.setGroup(query.getProcessType());
                   } 
                }
            });
        }, shard -> {
            return super.queryTask(req);
        });
    }

    @Action(code = "easyflow-process-canWithdraw", name = "canWithdraw")
    @Override
    public ExportResponse<CanWithdrawTaskRes> canWithdraw(ExportRequest<CanWithdrawTaskReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String taskNo = req.getData().getTaskNo();
                AssertUtils.isNotNull(taskNo);
                shardingDataQuerier.fillByTaskNo(taskNo, shardingData);
            });
        }, shard -> {
            return super.canWithdraw(req);
        });
    }

    @Action(code = "easyflow-process-withDraw", name = "withDraw")
    @Override
    public ExportResponse<WithdrawTaskRes> withDraw(ExportRequest<WithdrawTaskReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String taskNo = req.getData().getTaskNo();
                AssertUtils.isNotNull(taskNo);
                shardingDataQuerier.fillByTaskNo(taskNo, shardingData);
            });
        }, shard -> {
            return super.withDraw(req);
        });
    }

    @Action(code = "easyflow-process-doExecuteOperations", name = "doExecuteOperations")
    @Override
    public ExportResponse<TaskOperationsRes> doExecuteOperations(ExportRequest<TaskOperationsReq> req) {
        return super.doExecuteOperations(req);
    }

    @Action(code = "easyflow-process-findTaskAssignListByTaskNo", name = "findTaskAssignListByTaskNo")
    @Override
    public ExportResponse<List<ProcessTaskAssignDTO>> findTaskAssignListByTaskNo(ExportRequest<String> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String taskNo = req.getData();
                AssertUtils.isNotNull(taskNo);
                shardingDataQuerier.fillByTaskNo(taskNo, shardingData);
            });
        }, shard -> {
            return super.findTaskAssignListByTaskNo(req);
        });
    }

    public ProcessShardingDataQuerier getShardingDataQuerier() {
        return shardingDataQuerier;
    }

    public void setShardingDataQuerier(ProcessShardingDataQuerier shardingDataQuerier) {
        this.shardingDataQuerier = shardingDataQuerier;
    }

    public ShardingService getShardingService() {
        return shardingService;
    }

    public void setShardingService(ShardingService shardingService) {
        this.shardingService = shardingService;
    }

    public ExportRequestShardComputer getShardComputer() {
        return shardComputer;
    }

    public void setShardComputer(ExportRequestShardComputer shardComputer) {
        this.shardComputer = shardComputer;
    }
    

    
    
}
