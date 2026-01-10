package com.jd.easyflow.sharding.transaction;

import javax.sql.DataSource;

import org.apache.ibatis.session.TransactionIsolationLevel;
import org.apache.ibatis.transaction.Transaction;
import org.mybatis.spring.transaction.SpringManagedTransactionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.sharding.datasource.ShardingDataSource;

/**
 * @author liyuliang5
 */
public class ShardingSpringManagedTransactionFactory extends SpringManagedTransactionFactory {
    
    private static final Logger logger = LoggerFactory.getLogger(ShardingSpringManagedTransactionFactory.class);
    
    private boolean sharding = true;
    
    @Override
    public Transaction newTransaction(DataSource dataSource, TransactionIsolationLevel level, boolean autoCommit) {
        if (sharding) {
            if (dataSource instanceof ShardingDataSource) {
                ShardingDataSource shardingDataSource = (ShardingDataSource) dataSource;
                DataSource targetDataSource = shardingDataSource.determineTargetDataSourceInner();
                if (targetDataSource != null) {
                    dataSource = targetDataSource;
                } else {
                    if (logger.isWarnEnabled()) {
                        logger.warn("sharding targetDataSource is null," + dataSource.getClass());
                    }
                }
            } 
        }
        return super.newTransaction(dataSource, level, autoCommit);
    }

}
