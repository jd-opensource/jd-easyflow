package com.jd.easyflow.processunit.domain.support;

import java.util.Map;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.repository.ProcessUnitRepository;
import com.jd.easyflow.sharding.service.ShardingService;

/**
 * 
 * @author liyuliang5
 */
public class PuTransactionTemplate {

    private static final Logger log = LoggerFactory.getLogger(PuTransactionTemplate.class);

    @Autowired(required = false)
    @Qualifier(ProcessUnitConstants.BEAN_NEW_TX_TEMPLATE)
    protected TransactionTemplate transactionTemplate;
    @Autowired
    private ProcessUnitRepository processUnitRepository;
    
    private ShardingService shardingService;

    public <T> T doInTransaction(ProcessUnitEntity unit, String bizNo, String scene, Map<String, Object> extData,
            Supplier<T> supplier) {
        String txPolicy = null;
        Map<String, String> configSceneTxPolicy = (Map<String, String>) unit.getConfig(ProcessUnitConstants.CONF_TX_POLICY);
        if (scene != null && configSceneTxPolicy != null) {
            txPolicy = configSceneTxPolicy.get(scene);
        }
        txPolicy = txPolicy == null ? ProcessUnitConstants.TX_POLICY_NEW : txPolicy;
        if (transactionTemplate == null) {
            txPolicy = ProcessUnitConstants.TX_POLICY_NONE;
        }
        if (log.isDebugEnabled()) {
            log.debug("transactionTemplate:{}  final policy is {}", 
                    transactionTemplate, txPolicy);
        }
        if (ProcessUnitConstants.TX_POLICY_NEW.equals(txPolicy)) {
            if (shardingService == null || ! shardingService.isShardingEnabled()) {
                return transactionTemplate.execute((status) -> {
                    return supplier.get();
                });
            } else {
                if (unit == null || unit.getProcessUnitCode() == null || bizNo == null) {
                    log.error("unitCode and bizNo can not be null " + "unitCode:" + (unit == null ? null : unit.getProcessUnitCode()) + ", bizNo:" + bizNo);
                    return supplier.get();
                }
                return shardingService.execute(ShardingService.OP_TYPE_WRITE, unit.getProcessUnitCode(), bizNo, false, shard -> {
                    return transactionTemplate.execute((status) -> {
                        return supplier.get();
                    });
                });

            }
        } else {
            return supplier.get();
        }
    }

    public TransactionTemplate getTransactionTemplate() {
        return transactionTemplate;
    }

    public void setTransactionTemplate(TransactionTemplate transactionTemplate) {
        this.transactionTemplate = transactionTemplate;
    }

    public ProcessUnitRepository getProcessUnitRepository() {
        return processUnitRepository;
    }

    public void setProcessUnitRepository(ProcessUnitRepository processUnitRepository) {
        this.processUnitRepository = processUnitRepository;
    }

    public ShardingService getShardingService() {
        return shardingService;
    }

    public void setShardingService(ShardingService shardingService) {
        this.shardingService = shardingService;
    }
    
}
