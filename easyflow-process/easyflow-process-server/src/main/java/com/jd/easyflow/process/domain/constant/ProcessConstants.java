package com.jd.easyflow.process.domain.constant;

/**
 * @author liyuliang5
 *
 */
public class ProcessConstants {

    public static final String BEAN_LOCKER = "easyflow-process-locker";
    public static final String BEAN_CACHE_SERVICE = "easyflow-process-cacheService";
    public static final String BEAN_NEW_TX_TEMPLATE = "easyflow-process-newTransactionTemplate";
    public static final String BEAN_TX_MANAGER = "easyflow-process-transactionManager";
    public static final String BEAN_MESSAGE_SERVICE = "easyflow-process-messageSendService";
    public static final String TOPIC_PROCESS_INSTANCE_STATUS = "${easyflow.process.processInstanceStatusTopic:easyflow_process_processInstanceStatus}";
    public static final String TOPIC_NODE_INSTANCE_STATUS = "${easyflow.process.nodeInstanceStatusTopic:easyflow_process_nodeInstanceStatus}";
    public static final String CREATED_DATE_POLICY = "${easyflow.process.createdDatePolicy:client}";
    
    public static final String CREATED_DATE_POLICY_CLIENT = "client";
    public static final String CREATED_DATE_POLICY_SERVER = "server";
    public static final String CREATED_DATE_POLICY_DB = "db";
    
    public static final String TXN_ACTION_MSG = "msg";
    public static final String TXN_ACTION_MERGE_ASYNC = "mergeAsync";
}
