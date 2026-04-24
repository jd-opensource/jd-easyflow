package com.jd.easyflow.process.adapter.export.sharding;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.process.adapter.export.ProcessInstanceExportImpl;
import com.jd.easyflow.process.adapter.export.dto.instance.CanCancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CanCancelProcessInstanceRes;
import com.jd.easyflow.process.adapter.export.dto.instance.CancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CancelProcessInstanceRes;
import com.jd.easyflow.process.adapter.export.dto.instance.CreateProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CreateProcessInstanceRes;
import com.jd.easyflow.process.adapter.export.dto.instance.LockProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeExecutionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryOpenNodeInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessNodeReqDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.UnlockProcessInstanceReq;
import com.jd.easyflow.process.infrastructure.sharding.ProcessShardingDataQuerier;
import com.jd.easyflow.sharding.CurrentShardInfo;
import com.jd.easyflow.sharding.service.ExportRequestShardComputer;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 * @author liyuliang5
 */
public class ProcessInstanceExportShardingImpl extends ProcessInstanceExportImpl {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessInstanceExportShardingImpl.class);

    
    private ProcessShardingDataQuerier shardingDataQuerier;
    
    private ShardingService shardingService;
    
    private ExportRequestShardComputer shardComputer;

    @Action(code = "easyflow-process-createProcessInstance", name = "createProcessInstance")
    public ExportResponse<CreateProcessInstanceRes> createProcessInstance(ExportRequest<CreateProcessInstanceReq> req) {
        return super.createProcessInstance(req);
    }

    @Action(code = "easyflow-process-getProcessInstance", name = "getProcessInstance")
    @Override
    public ExportResponse<ProcessInstanceDTO> getProcessInstance(ExportRequest<String> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String instanceNo = req.getData();
                AssertUtils.isNotNull(instanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(instanceNo, shardingData);
            });
        }, () -> {
            return ExportResponse.build4Success();
        }, shard -> {
            return super.getProcessInstance(req);
        });
    }

    @Action(code = "easyflow-process-pagerQueryProcessInstance", name = "pagerQueryProcessInstance")
    @Override
    public ExportResponse<PagerResult> pagerQueryProcessInstance(ExportRequest<PagerCondition> req) {
        if (! shardingService.isShardingEnabled()) {
            return super.pagerQueryProcessInstance(req);
        }
        CurrentShardInfo shardInfo = shardComputer.computeExportRequestShard(req,  shardingData->{
            PagerCondition condition = req.getData();
            String processType = condition.getField("processType") == null ? null : (String) condition.getField("processType").getValue();
            String bizNo = condition.getField("bizNo") == null ? null : (String) condition.getField("bizNo").getValue();
            String processInstanceNo = condition.getField("instanceNo") == null ? null : (String) condition.getField("instanceNo").getValue();
            if (processType != null && bizNo != null) {
                shardingData.setGroup(processType);
                shardingData.setBizNo(bizNo);
            } else if (processInstanceNo != null) {
                shardingDataQuerier.fillByProcessInstanceNo(processInstanceNo, shardingData);
            } else if (processType != null) {
               if (shardingService.getShardListOfGroup(processType).size() == 1) {
                   shardingData.setGroup(processType);
               }
            }               
        });
        if (shardInfo != null) {
            req.getData().addSortField("instanceNo", 0, "desc");
            return shardingService.executeInShard(shardInfo, info -> {
                return super.pagerQueryProcessInstance(req);
            });
        } else {
            return shardingService.parallelExecuteInAllShard(false, shard -> {
                return super.pagerQueryProcessInstance(req);
            }, subList -> {
                long count = 0;
                List<ProcessInstanceDTO> result = new ArrayList<>();
                for (ExportResponse<PagerResult> res : subList) {
                    PagerResult pagerResult = ExportResponseUtil.unwrap(res);
                    count += pagerResult.getCount() == null ? 0 : pagerResult.getCount();
                    result.addAll(pagerResult.getList());
                }
                Collections.sort(result, (instance1, instance2) -> {
                    return -instance1.getInstanceNo().compareTo(instance2.getInstanceNo());
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

    @Action(code = "easyflow-process-queryProcessNodeInstanceByInstanceNo", name = "queryProcessNodeInstanceByInstanceNo")
    @Override
    public ExportResponse<List<ProcessNodeInstanceDTO>> queryProcessNodeInstanceByInstanceNo(
            ExportRequest<String> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String processInstanceNo = req.getData();
                AssertUtils.isNotNull(processInstanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(processInstanceNo, shardingData);
            });
        }, () -> {
            return ExportResponse.build4Success(new ArrayList());
        }, shard -> {
            return super.queryProcessNodeInstanceByInstanceNo(req);
        });
    }

    @Action(code = "easyflow-process-queryProcessInstanceByProcessTypeAndBizNo", name = "queryProcessInstanceByProcessTypeAndBizNo")
    @Override
    public ExportResponse<ProcessInstanceDTO> queryProcessInstanceByProcessTypeAndBizNo(
            ExportRequest<QueryProcessInstanceReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String processType = req.getData().getProcessType();
                String bizNo = req.getData().getBizNo();
                shardingData.setGroup(processType);
                shardingData.setBizNo(bizNo);
            });
        }, shard -> {
            return super.queryProcessInstanceByProcessTypeAndBizNo(req);
        });
    }

    @Action(code = "easyflow-process-lockProcessInstance", name = "lockProcessInstance")
    @Override
    public ExportResponse<String> lockProcessInstance(ExportRequest<LockProcessInstanceReq> req) {
        return super.lockProcessInstance(req);
    }

    @Action(code = "easyflow-process-unLockProcessInstance", name = "unLockProcessInstance")
    @Override
    public ExportResponse<Boolean> unLockProcessInstance(ExportRequest<UnlockProcessInstanceReq> req) {
        return super.unLockProcessInstance(req);
    }

    @Action(code = "easyflow-process-queryOpenNodeInstance", name = "queryOpenNodeInstance")
    @Override
    public ExportResponse<ProcessNodeInstanceDTO> queryOpenNodeInstance(
            ExportRequest<QueryOpenNodeInstanceReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String instanceNo = req.getData().getInstanceNo();
                AssertUtils.isNotNull(instanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(instanceNo, shardingData);
            });
        }, ()-> {
            return ExportResponse.build4Success(null);
        }, shard -> {
            return super.queryOpenNodeInstance(req);
        });
    }

    @Action(code = "easyflow-process-findNodeInstances", name = "findNodeInstances")
    @Override
    public ExportResponse<List<ProcessNodeInstanceDTO>> findNodeInstances(
            ExportRequest<QueryProcessNodeReqDTO> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String instanceNo = req.getData().getProcessInstanceNo();
                AssertUtils.isNotNull(instanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(instanceNo, shardingData);
            });
        }, ()-> {
            return ExportResponse.build4Success(new ArrayList());
        }, shard -> {
            return super.findNodeInstances(req);
        });
    }

    @Action(code = "easyflow-process-queryNodeInstanceByNo", name = "queryNodeInstanceByNo")
    @Override
    public ExportResponse<ProcessNodeInstanceDTO> queryNodeInstanceByNo(ExportRequest<String> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String nodeInstanceNo = req.getData();
                AssertUtils.isNotNull(nodeInstanceNo);
                shardingDataQuerier.fillByProcessNodeInstanceNo(nodeInstanceNo, shardingData);
            });
        }, ()-> {
            return ExportResponse.build4Success(null);
        }, shard -> {
            return super.queryNodeInstanceByNo(req);
        });
    }

    @Action(code = "easyflow-process-queryNodeExecutionByNo", name = "queryNodeExecutionByNo")
    @Override
    public ExportResponse<ProcessNodeExecutionDTO> queryNodeExecutionByNo(ExportRequest<String> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String nodeInstanceNo = req.getData();
                AssertUtils.isNotNull(nodeInstanceNo);
                shardingDataQuerier.fillByProcessNodeInstanceNo(nodeInstanceNo, shardingData);
            });
        }, ()-> {
            return ExportResponse.build4Success(null);
        }, shard -> {
            return super.queryNodeExecutionByNo(req);
        });
    }

    @Action(code = "easyflow-process-updateProcessInstance", name = "updateProcessInstance")
    @Override
    public ExportResponse<Object> updateProcessInstance(ExportRequest<ProcessInstanceDTO> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String processInstanceNo = req.getData().getInstanceNo();
                AssertUtils.isNotNull(processInstanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(processInstanceNo, shardingData);
            });
        }, ()-> {
            throw new IllegalArgumentException("instance not exists");
        }, shard -> {
            return super.updateProcessInstance(req);
        });
    }

    @Action(code = "easyflow-process-queryActiveProcessInstanceByProcessTypeAndBizNo", name = "queryActiveProcessInstanceByProcessTypeAndBizNo")
    @Override
    public ExportResponse<ProcessInstanceDTO> queryActiveProcessInstanceByProcessTypeAndBizNo(
            ExportRequest<QueryProcessInstanceReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                shardingData.setGroup(req.getData().getProcessType());
                shardingData.setBizNo(req.getData().getBizNo());
            });
        }, shard -> {
            return super.queryActiveProcessInstanceByProcessTypeAndBizNo(req);
        });
    }

    @Action(code = "easyflow-process-canCancel", name = "canCancel")
    @Override
    public ExportResponse<CanCancelProcessInstanceRes> canCancel(ExportRequest<CanCancelProcessInstanceReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String instanceNo = req.getData().getInstanceNo();
                AssertUtils.isNotNull(instanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(instanceNo, shardingData);
            });
        }, shard -> {
            return super.canCancel(req);
        });
    }

    @Action(code = "easyflow-process-cancel", name = "cancel")
    @Override
    public ExportResponse<CancelProcessInstanceRes> cancel(ExportRequest<CancelProcessInstanceReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req,  shardingData->{
                String instanceNo = req.getData().getInstanceNo();
                AssertUtils.isNotNull(instanceNo);
                shardingDataQuerier.fillByProcessInstanceNo(instanceNo, shardingData);
            });
        }, shard -> {
            return super.cancel(req);
        });
    }

    @Override
    @Action(code = "easyflow-process-queryInstanceByInstanceNos", name = "queryInstanceByInstanceNos")
    public ExportResponse<List<ProcessInstanceDTO>> queryInstanceByInstanceNos(ExportRequest<List<String>> request) {
        if (! shardingService.isShardingEnabled()) {
            return super.queryInstanceByInstanceNos(request);
        }
        CurrentShardInfo shard =shardComputer.computeExportRequestShard(request,  shardingData->{
            if (request.getData().size() == 1) {
            shardingDataQuerier.fillByProcessInstanceNo(request.getData().get(0), shardingData);
            }
        });
        if (shard != null) {
            return shardingService.executeInShard(shard, info -> {
                return super.queryInstanceByInstanceNos(request);
            });
        } else {
            return shardingService.parallelExecuteInAllShard(false, shardInfo -> {
                return super.queryInstanceByInstanceNos(request);
            }, subList -> {
                List<ProcessInstanceDTO> result = new ArrayList<ProcessInstanceDTO>();
                for (ExportResponse<List<ProcessInstanceDTO>> res : subList) {
                    result.addAll(res.getData());
                }
                return ExportResponse.build4Success(result);
            });
        }
    }

    @Action(code = "easyflow-process-queryNodeInstanceByNos", name = "queryNodeInstanceByNos")
    @Override
    public ExportResponse<List<ProcessNodeInstanceDTO>> queryNodeInstanceByNos(ExportRequest<List<String>> request) {
        if (! shardingService.isShardingEnabled()) {
            return super.queryNodeInstanceByNos(request);
        }
        CurrentShardInfo shard =shardComputer.computeExportRequestShard(request,  shardingData->{
            if (request.getData().size() == 1) {
            shardingDataQuerier.fillByProcessNodeInstanceNo(request.getData().get(0), shardingData);
            }
        });
        if (shard != null) {
            return shardingService.executeInShard(shard, info -> {
                return super.queryNodeInstanceByNos(request);
            });
        } else {
            return shardingService.parallelExecuteInAllShard(false, shardInfo -> {
                return super.queryNodeInstanceByNos(request);
            }, subList -> {
                List<ProcessNodeInstanceDTO> result = new ArrayList<ProcessNodeInstanceDTO>();
                for (ExportResponse<List<ProcessNodeInstanceDTO>> res : subList) {
                    result.addAll(res.getData());
                }
                return ExportResponse.build4Success(result);
            });
        }
    }
    
    @Action(code = "easyflow-process-queryProcessInstanceByParentInstanceNo", name = "queryProcessInstanceByParentInstanceNo")
    @Override
    public ExportResponse<List<ProcessInstanceDTO>> queryProcessInstanceByParentInstanceNo(ExportRequest<String> request) {
        if (! shardingService.isShardingEnabled()) {
            return super.queryProcessInstanceByParentInstanceNo(request);
        }
        CurrentShardInfo shard =shardComputer.computeExportRequestShard(request,  shardingData->{
        });
        if (shard != null) {
            return shardingService.executeInShard(shard, info -> {
                return super.queryProcessInstanceByParentInstanceNo(request);
            });
        } else {
            return shardingService.parallelExecuteInAllShard(false, shardInfo -> {
                return super.queryProcessInstanceByParentInstanceNo(request);
            }, subList -> {
                List<ProcessInstanceDTO> result = new ArrayList<ProcessInstanceDTO>();
                for (ExportResponse<List<ProcessInstanceDTO>> res : subList) {
                    result.addAll(res.getData());
                }
                return ExportResponse.build4Success(result);
            });
        }
    }
    
    @Action(code = "easyflow-process-pagerQueryNodeExecution", name = "pagerQueryNodeExecution")
    @Override
    public ExportResponse<PagerResult> pagerQueryNodeExecution(ExportRequest<PagerCondition> req) {
        if (! shardingService.isShardingEnabled()) {
            return super.pagerQueryNodeExecution(req);
        }
        CurrentShardInfo shard =shardComputer.computeExportRequestShard(req,  shardingData->{
            PagerCondition condition = req.getData();
            List<String> nodeInstanceNoList = condition.getField("nodeInstanceNoList") == null ? null : (List<String>) condition.getField("nodeInstanceNoList").getValue();
            String nodeExecutionNo = condition.getField("nodeExecutionNo") == null ? null : (String) condition.getField("nodeExecutionNo").getValue();
            if (nodeInstanceNoList != null && nodeInstanceNoList.size() == 1) {
                shardingDataQuerier.fillByProcessNodeInstanceNo(nodeInstanceNoList.get(0), shardingData);
            } else if (nodeExecutionNo != null) {
                shardingDataQuerier.fillByProcessNodeExecutionNo(nodeExecutionNo, shardingData);
            } else {
                throw new UnsupportedOperationException();
            }
        });
        if (shard != null) {
            return shardingService.executeInShard(shard, info -> {
                return super.pagerQueryNodeExecution(req);
            });
        }
        throw new UnsupportedOperationException();
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
