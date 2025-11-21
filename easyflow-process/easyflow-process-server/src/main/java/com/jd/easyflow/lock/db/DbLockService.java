package com.jd.easyflow.lock.db;

import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;
import javax.sql.DataSource;

import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.lock.impl.BaseLockService;


/**
 * @author liyuliang5
 */
public class DbLockService extends BaseLockService {

    private static final String LOCK_ON = "1";
    private static final String LOCK_OFF = "0";

    private JdbcTemplate jdbcTemplate;
    
    private TransactionTemplate transactionTemplate;

    private DataSource dataSource;
    
    private String selectForUpdateSql = "select request_id,lock_flag from lock_record where lock_key=? for update";
    
    private String insertSql = "insert into lock_record(lock_key, lock_flag, request_id, expired_time) values(?,?,?,?)";
    
    private String lockUpdateSql = "update lock_record set lock_flag='1',request_id=?,expired_time=? where lock_key=?";
    
    private String unlockUpdateSql = "update lock_record set lock_flag='0' where lock_key=? and request_id=? and lock_flag='1'";
    
    private String renewSql = "update lock_record set expired_time=? where lock_key=? and request_id=? and lock_flag='1'";
    
    @PostConstruct
    public void init() {
        jdbcTemplate = new JdbcTemplate(dataSource);
        DataSourceTransactionManager transactionManager = new DataSourceTransactionManager(dataSource);
        transactionTemplate = new TransactionTemplate(transactionManager);
        transactionTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
    }
    
    /**
     * 
     * @param key
     * @param requestId
     * @param lockSeconds. not use.
     * @return
     */
    @Override
    protected boolean accquireLock(String key, String requestId, int lockSeconds) {
        return transactionTemplate.execute(status -> {
            List<Map<String, Object>> lockRecordList = jdbcTemplate.queryForList(selectForUpdateSql, key);
            long currentTime = System.currentTimeMillis();
            long expiredTime = currentTime + lockSeconds * 1000;
            if (lockRecordList.size() == 0) {
                try {
                    jdbcTemplate.update(insertSql, key, LOCK_ON, requestId, expiredTime);
                    return true;
                } catch (DuplicateKeyException e) {
                    logger.error("key duplicate " + e.getMessage(), e);
                    lockRecordList = jdbcTemplate.queryForList(selectForUpdateSql, key);
                }
            }
            Map<String, Object> lockRecord = lockRecordList.get(0);
            String lockFlag = (String) lockRecord.get("lock_flag");
            Number currentExpiredTime = (Number) lockRecord.get("expired_time");
            long expireTime = currentExpiredTime == null ? 0 : currentExpiredTime.longValue();
            if (LOCK_ON.equals(lockFlag) && expireTime <= currentTime) {
                return false;
            } else {
                int count = jdbcTemplate.update(lockUpdateSql, requestId, expiredTime, key);
                return count == 1;
            }
        });
    }
    
    @Override
    protected boolean releaseLock(String key, String requestId) {
        return transactionTemplate.execute(status -> {
            int count = jdbcTemplate.update(unlockUpdateSql, key, requestId);
            return count == 1;
        });
    }
    
    @Override
    protected String queryLock(String key) {
        return transactionTemplate.execute(status -> {
            List<Map<String, Object>> lockRecordList = jdbcTemplate.queryForList(selectForUpdateSql, key);
            return lockRecordList.size() == 0 ? null : (String) lockRecordList.get(0).get("request_id");
        });
    }
    
    protected boolean renewLock(String key, String requestId, int renewSeconds) {
        return transactionTemplate.execute(status -> {
            long expiredTime = System.currentTimeMillis() + renewSeconds * 1000;
            int count = jdbcTemplate.update(renewSql, expiredTime, key, requestId);
            return count == 1;
        });
    }


    public DataSource getDataSource() {
        return dataSource;
    }


    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    public String getSelectForUpdateSql() {
        return selectForUpdateSql;
    }

    public void setSelectForUpdateSql(String selectForUpdateSql) {
        this.selectForUpdateSql = selectForUpdateSql;
    }

    public String getInsertSql() {
        return insertSql;
    }

    public void setInsertSql(String insertSql) {
        this.insertSql = insertSql;
    }

    public String getLockUpdateSql() {
        return lockUpdateSql;
    }

    public void setLockUpdateSql(String lockUpdateSql) {
        this.lockUpdateSql = lockUpdateSql;
    }

    public String getUnlockUpdateSql() {
        return unlockUpdateSql;
    }

    public void setUnlockUpdateSql(String unlockUpdateSql) {
        this.unlockUpdateSql = unlockUpdateSql;
    }

    public JdbcTemplate getJdbcTemplate() {
        return jdbcTemplate;
    }

    public void setJdbcTemplate(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    public TransactionTemplate getTransactionTemplate() {
        return transactionTemplate;
    }

    public void setTransactionTemplate(TransactionTemplate transactionTemplate) {
        this.transactionTemplate = transactionTemplate;
    }

    public String getRenewSql() {
        return renewSql;
    }

    public void setRenewSql(String renewSql) {
        this.renewSql = renewSql;
    }



}
