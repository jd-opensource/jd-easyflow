package com.jd.easyflow.processunit.domain.constant;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessUnitConstants {

    public static final String RESULT_SUCCESS = "SUCCESS";
    
    public static final String RESULT_FAIL = "FAIL";
    
    public static final String RESULT_DOING = "DOING";
    
    public static final String RESULT_EXCEPTION = "EXCEPTION";
    
    public static final String RESULT_UNKNOWN = "UNKNOWN";
    
    public static final String RESULT_NONE = "NONE";
    
    public static final String POLICY_OLD = "OLD";
    public static final String POLICY_REQUEST = "REQUEST";
    public static final String POLICY_EXCEPTION = "EXCEPTION";
    
    public static final String REQ_TYPE_NEW = "NEW";
    
    public static final String REQ_TYPE_SUCCESS = "SUCCESS";
    
    public static final String REQ_TYPE_FAIL = "FAIL";
    
    public static final String REQ_TYPE_DOING = "DOING";
    
    public static final String REQ_TYPE_EXCEPTION = "EXCEPTION";
    
    public static final String CONF_SYNC_EXEC_POLICY = "syncExecPolicy";
    public static final String CONF_ASYNC_EXEC_POLICY = "asyncExecPolicy";
    public static final String CONF_CATCH_EXCEPTION = "catchException";
    public static final String CONF_AUTO_ASYNC_RUN = "autoAsyncRun";
    public static final String CONF_AUTO_ASYNC_RUN_PERIOD = "autoAsyncRunPeriod";
    public static final String CONF_AUTO_ASYNC_RUN_PERIOD_POWER_MAX_TIME = "autoAsyncRunPeriodPowerMaxTime";
    public static final String CONF_AUTO_ASYNC_MAX_TIMES = "autoAsyncMaxTimes";
    public static final String CONF_AUTO_RUN_POLICY = "autoRunPolicy";
    public static final String CONF_AUTO_RUN_DELAY_SECONDS = "autoRunDelaySeconds";

    
    public static final String CONF_ASYNC_RUN_EXP = "asyncRunExp";
    
    public static final String CONF_ASYNC_PROVIDER_ID = "asyncProviderId";
    public static final String CONF_ASYNC_SERVICE_ID = "asyncServiceId";
    public static final String CONF_ASYNC_SERVICE_CONF = "asyncServiceConf";
    
    public static final String CONF_LOCK_SECONDS = "lockSeconds";
    
    public static final String CONF_TX_POLICY = "txPolicy";
    
    public static final String CONF_SHARDING = "sharding";
    
    public static final String CONF_SHARDING_LIST = "list";
    
    public static final String CONF_SHARDING_SLAVE_LIST = "slaveList";
    public static final String CONF_SHARDING_DOUBLE_WRITE = "doubleWrite";
    public static final String CONF_SHARDING_IGNORE_SLAVE_ERROR = "ignoreSlaveError";
    
    public static final String CONF_ALERT_RULES = "alertRules";
    
    public static final String TX_POLICY_NONE = "none";
    
    public static final String TX_POLICY_NEW = "new";
    
    public static final int DEFAULT_LOCK_SECONDS = 60 * 60 * 24;
    public static final int DEFAULT_WAIT_SECONDS = 10;
    
    public static final String STATUS_VALID = "VALID";
    public static final String STATUS_INVALID = "INVALID";
    
    public static final String LOCK_PREFIX = "PU_";
    

    public static final String EXEC_TYPE_SYNC = "SYNC";
    public static final String EXEC_TYPE_MANUAL_ASYNC = "MANUAL_ASYNC";
    public static final String EXEC_TYPE_TASK_ASYNC = "TASK_ASYNC";
    public static final String EXEC_TYPE_MSG_ASYNC = "MSG_ASYNC";
    public static final String EXEC_TYPE_AFTER_CREATE = "AFTER_CREATE";
    
    public static final String EXECUTION_EXT_DATA_LOCK_REQUEST_ID = "_lockRequestId";
    public static final String EXECUTION_EXT_DATA_POLICY_TYPE = "_policyType";
    public static final String EXECUTION_EXT_DATA_CLIENT_INFO = "_clientInfo";
    public static final String EXECUTION_EXT_DATA_CALL_TYPE = "_reqType";
    public static final String EXECUTION_EXT_DATA_BEFORE_VARS = "_before_vars";
    public static final String EXECUTION_EXT_DATA_AFTER_VARS = "_after_vars";
    
    public static final String CALL_TYPE_UPDATE = "UPDATE";
    
    public static final String EXECUTION_REQUEST_CONTEXT = "_requestContext";
    
    public static final String INSTANCE_EXT_DATA_CREATE_CLIENT_INFO = "_createClientInfo";
    
    public static final String INSTANCE_EXT_DATA_UPDATE_AUTO_RUN_TIME = "_updateAutoRunTime";

    
    public static final String CONF_CREATE_EXEC_POLICY = "createExecPolicy";
    public static final String CONF_CREATE_EXEC_MQ_TOPIC = "PU_CREATE_MESSAGE";
 
    public static final String CONF_UPDATE_EXEC_POLICY = "updateExecPolicy";
    public static final String CONF_UPDATE_EXEC_MQ_TOPIC = "PU_UPDATE_MESSAGE";
    
    public static final String CONF_EXECUTE_MESSAGE_TOPIC = "executeMessageTopic";
    
    public static final String LOCK_BIZ_TYPE = "_PU";
    
    public static final String LOCK_KEY_SEP = "--";

    public static final String EXECUTION_PERSIST_POLICY = "executionPersistPolicy";

    public static final String PU_PERSIST_HANDLER = "handler";
    
    public static final String EXECUTION_PERSIST_TYPE_DEFAULT = "DEFAULT";

    public static final String DUMMY_PERSIST_HANDLER_NAME = "dummyPersistHandler";
    public static final String SYNC_PERSIST_HANDLER_NAME = "syncPersistHandler";
    public static final String THREAD_POOL_ASYNC_PERSIST_HANDLER_NAME = "threadPoolAsyncPersistHandler";
    
    public static final String BATCH_RUN_POLICY = "batchRunPolicy";
    public static final String BATCH_RUN_POLICY_CONF = "batchRunPolicyConf";
    public static final String BATCH_RUN_POLICY_MULTIPLE_THREAD = "multipleThread";
    
    public static final String BEAN_NEW_TX_TEMPLATE = "easyflow-processunit-newTransactionTemplate";
    
    public static final String BEAN_PU_TX_TEMPLATE = "easyflow-processunit-puTransactionTemplate";
    
    public static final String INSTANCE_QUERY_TYPE_SINGLE = "SINGLE";
    
    public static final String INSTANCE_QUERY_TYPE_CREATED_DATE_RANGE = "CREATED_DATE_RANGE";
    
    public static final String EXECUTION_QUERY_TYPE_INSTANCE = "INSTANCE";
    
    public static final String EXECUTION_QUERY_TYPE_REQUEST_TIME_RANGE = "REQUEST_TIME_RANGE";

    public static final String ALERT_CODE_ASYNC_RESULT = "processunit-async";
    public static final String ALERT_CODE_ASYNC_MAX_TIMES = "processUnit-asyncExceptionExceedMaxTimes";
    
    public static final String SCENE_CREATE = "CREATE";
    public static final String SCENE_SYNC = "SYNC";
    public static final String SCENE_ASYNC = "ASYNC";
    public static final String SCENE_EXECUTE = "EXECUTE";
    public static final String SCENE_UPDATE = "UPDATE";
    
}
