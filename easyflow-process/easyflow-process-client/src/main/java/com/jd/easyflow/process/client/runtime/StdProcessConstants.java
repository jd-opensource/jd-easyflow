package com.jd.easyflow.process.client.runtime;

/**
 * @author liyuliang5
 * 
 */
public class StdProcessConstants {
    
    public static final String NS_SEP = ":";
    public static final String NS_EASYFLOW = "easyflow";

    public static final String FLUSH_AFTER_PROCESS = "AFTER_PROCESS";
    public static final String FLUSH_BEFORE_NODE = "BEFORE_NODE";
    public static final String FLUSH_AFTER_NODE = "AFTER_NODE";
    public static final String FLUSH_BEFORE_AND_AFTER_NODE = "BEFORE_AND_AFTER_NODE";
    
    public static final String STATUS_ACTIVE = "ACTIVE";
    public static final String STATUS_CLOSE = "CLOSE";
    public static final String STATUS_CANCELED = "CANCELED";
    
    public static final String NODE_STATUS_ACTIVE = "ACTIVE";
    public static final String NODE_STATUS_INACTIVE = "INACTIVE";
    public static final String NODE_STATUS_CLOSE = "CLOSE";
    public static final String NODE_STATUS_INVALID = "INVALID";
    
    public static final String EXECUTION_STATUS_CLOSE = "CLOSE";
    
    public static final String CTX_VARIABLES = "_VARIABLES";
    
    public static final String NODE_CTX_VARIABLES = "_VARIABLES";

    
    public static final String PROP_START = "start";
    public static final String PROP_END = "end";
    
    public static final String PROCESS_DEF_FSM = "FSM-";
    public static final String PROCESS_DEF_FLOW = "FLOW-";
    
    public static final String PROP_DATA_FLUSH_POLICY = "dataFlushPolicy";
    public static final String PROP_CHECK_FLUSH_NODES = "checkFlushNodes";
    public static final String PROP_FLUSH = "flush";
    public static final String PROP_FLUSH_NODES = "flushNodes";
    public static final String PROP_START_NODE_IDS = "startNodeIds";
    public static final String PROP_END_NODE_IDS = "endNodeIds";
    public static final String PROP_CHECK_START_NODE = "checkStartNode";
    public static final String PROP_PROCESS_INSTANCE_STATUS_MESSAGE = "processInstanceStatusMessage";
    public static final String PROP_NODE_INSTANCE_STATUS_MESSAGE = "nodeInstanceStatusMessage";
    public static final String PROP_TASK = "task";
    
    public static final String EXT_PROP_ENGINE = "_engine";
    public static final String ENGINE_FLOW = "flow";
    public static final String ENGINE_FSM = "fsm";
    
    public static final String PROP_CREATE_INSTANCE_EVERY_TIME = "createInstanceEveryTime";
    public static final String PROP_SAVE_PREVIOUS_POLICY = "savePreviousPolicy";
    public static final String PROP_START_CHECK_POLICY = "startCheckPolicy";
    public static final String PROP_BIZ_RELATION = "bizRelation";
    public static final String PROP_EXECUTE_CLOSE_POLICY = "executeClosePolicy";
    public static final String PROP_DATA_FLUSH_AFTER_CREATE = "dataFlushAfterCreate";
    
    public static final String PROP_DATA_FLUSH_BY_PARENT = "dataFlushByParent";
    
    public static final String BIZ_RELATION_ONE_ONE = "ONE_ONE";
    public static final String BIZ_RELATION_MANY_ONE = "MANY_ONE";
    
    public static final String POLICY_EXCEPTION = "EXCEPTION";
    public static final String POLICY_EMPTY_RUN = "EMPTY_RUN";
    public static final String POLICY_NORMAL = "NORMAL";
        
    public static final String PROP_BIZNO = "bizNo";
    public static final String PROP_PROCESS_TYPE = "processType";
    
    public static final String SAVE_PREVIOUS_POLICY_ALL = "ALL";
    public static final String SAVE_PREVIOUS_POLICY_NONE = "NONE";
    public static final String SAVE_PREVIOUS_POLICY_DISTINCT = "DISTINCT";

    public static final String START_CHECK_POLICY_ONCE = "ONCE";
    public static final String START_CHECK_POLICY_UNLIMIT = "UNLIMIT";
    public static final String START_CHECK_POLICY_NO_OPENING = "NO_OPENING";
    

    public static final String OP_TYPE_CREATE = "CREATE";
    public static final String OP_TYPE_EXECUTE = "EXECUTE";
    public static final String OP_TYPE_CREATE_OR_EXECUTE = "CREATE_OR_EXECUTE";
    
    public static final String EVENT_PROCESS_INSTANCE_START = "PROCESS_INSTANCE_START";
    public static final String EVENT_PROCESS_INSTANCE_END = "PROCESS_INSTANCE_END";
    
    public static final String EVENT_NODE_INSTANCE_START = "NODE_INSTANCE_START";
    public static final String EVENT_NODE_INSTANCE_END = "NODE_INSTANCE_END";
    
    public static final String EVENT_TXN_FLUSH_START = "TXN_FLUSH_START";
    public static final String EVENT_TXN_FLUSH_END = "TXN_FLUSH_END";
    
    public static final int NODE_START_EVENT_POLICY_ACTIVE = 1;
    public static final int NODE_START_EVENT_POLICY_CREATE = 2;
    
    public static final String PROCESS_VAR_COMPENSATE_FLAG = "_COMPENSATE_FLAG";
    public static final String PROCESS_VAR_COMPENSATED_FLAG = "_COMPENSATED_FLAG";
    public static final String NODE_VAR_COMPENSATED_FLAG = "_COMPENSATED_FLAG";
    public static final String NODE_VAR_COMPENSATE_NODE_FLAG = "_COMPENSATE_NODE_FLAG";
    public static final String NODE_VAR_COMPENSATE_FOR = "_COMPENSATE_FOR";
    public static final String NODE_VAR_COMPENSATED_BY = "_COMPENSATED_BY";
    
}
