package com.jd.easyflow.process.adapter.export.constant;

/**
 * @author liyuliang5
 *
 */
public class ProcessTransactionConstants {
    
    public static final String TYPE_PROCESS = "PROCESS";
    public static final String TYPE_NODE = "NODE";
    public static final String TYPE_EXECUTION = "EXECUTION";
    public static final String TYPE_TASK = "TASK";
    public static final String TYPE_TASK_ASSIGN = "TASK_ASSIGN";
    public static final String TYPE_TASK_EVENT = "TASK_EVENT";
    
    public static final int PERSIST_OP_NONE = 0;
    public static final int PERSIST_OP_ADD = 1;
    public static final int PERSIST_OP_UPDATE = 2;

    public static final String TXN_COMMAND_BATCH_UPDATE = "batchUpdate";

    public static final String TXN_COMMAND_INTERRUPT = "interrupt";

}
