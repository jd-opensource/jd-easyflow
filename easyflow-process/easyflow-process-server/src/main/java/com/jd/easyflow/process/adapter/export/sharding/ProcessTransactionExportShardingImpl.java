package com.jd.easyflow.process.adapter.export.sharding;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.alert.AlertUtil;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.process.adapter.export.ProcessTransactionExportImpl;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdRes;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnRes;
import com.jd.easyflow.process.domain.constant.ProcessConstants;
import com.jd.easyflow.process.infrastructure.sharding.ProcessShardingDataQuerier;
import com.jd.easyflow.sharding.service.ExportRequestShardComputer;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 * @author liyuliang5
 */
public class ProcessTransactionExportShardingImpl extends ProcessTransactionExportImpl {

    private static final Logger log = LoggerFactory.getLogger(ProcessTransactionExportShardingImpl.class);

    private ProcessShardingDataQuerier shardingDataQuerier;

    private ShardingService shardingService;

    private ExportRequestShardComputer shardComputer;

    @Action(code = "easyflow-process-nextObjectId", name = "nextObjectId")
    @Override
    public ExportResponse<String> nextObjectId(ExportRequest<String> request) {
        return super.nextObjectId(request);
    }

    @Action(code = "easyflow-process-batchNextObjectId", name = "batchNextObjectId")
    @Override
    public ExportResponse<BatchObjectIdRes> batchNextObjectId(ExportRequest<BatchObjectIdReq> request) {
        return super.batchNextObjectId(request);
    }

    @Action(code = "easyflow-process-doTransaction", name = "doTransaction")
    @Override
    public ExportResponse<TxnRes> doTransaction(ExportRequest<TxnReq> req) {
        return shardingService.executeInShard(() -> {
            return shardComputer.computeExportRequestShard(req, shardingData -> {
                throw new IllegalArgumentException("no shard info");
            });
        }, shard -> {
            List<Map<String, Object>> postActionList = new ArrayList<>();
            TxnRes response = transactionTemplate.execute(status -> {
                return executeTransaction(req.getData(), postActionList);
            });
            for (Map<String, Object> action : postActionList) {
                String type = (String) action.get("type");
                if (ProcessConstants.TXN_ACTION_MERGE_ASYNC.equals(type)) {
                    List<Runnable> tasks = (List<Runnable>) action.get("tasks");
                    executor.execute(() -> {
                        shardingService.executeInShard(shard, shardInfo -> {
                            if (log.isDebugEnabled()) {
                                log.debug("Start async process");
                            }
                            try {
                                tasks.forEach(task -> task.run());
                                if (log.isDebugEnabled()) {
                                    log.debug("End async process");
                                }
                            } catch (Exception e) {
                                AlertUtil.alert("Async process exception", e);
                            }
                            return null;
                        });
                    });
                } else {
                    String topic = (String) action.get("topic");
                    String bizData = (String) action.get("bizData");
                    messageSendService.sendMessage(UUID.randomUUID().toString(), topic, bizData);
                }
            }
            return ExportResponse.build4Success(response);
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
