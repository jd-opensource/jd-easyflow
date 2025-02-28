package com.jd.easyflow.flow.util;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowConstants {
    
    public static final String NS_SEP = ":";
    
    public static final String NS_EASYFLOW = "easyflow";
    
    public static final String EASYFLOW_NS_PREFIX = NS_EASYFLOW + NS_EASYFLOW;

	/**
	 * Param keys.
	 */
    public static final String PARAM_ACTION_EXECUTOR = "actionExecutor";
	/**
	 * Record execute history, default is true
	 */
	public static final String FLOW_PROPERTY_RECORD_HISTORY = "flow.recordHistory";
	
	/**
	 * Node action filter.
	 */
	public static final String FLOW_NODE_ACTION_FILTERS = "flow.nodeActionFilters";
	
    public static final int EVENT_ORDER_START = 10000;
    
    /**
     * Exceptions in multiple thread scenario.
     */
    public static final String FLOW_CTX_MULTI_EXCEPTIONS = "_flow.multi.exceptions";
    
    public static final String FLOW_CTX_MULTI_AWAIT_RESULT = "_flow.multi.await.result";
    
    public static final String NODE_CTX_MULTI_EXCEPTION = "_flow.multi.exception";
    
    
    public static final String CTX_LOCK_PREFIX = "_node_lock_";
    
    public static final String CTX_PRE_NODES_PREFIX = "_pre_nodes_";
    
    public static final String CTX_PARENT_CONTEXT = "_parentContext";
    
    public static final String CTX_PARENT_NODE_CONTEXT = "_parentNodeContext";
    
    public static final String NODECTX_PRE_RESULT = "_preResult";
    
    public static final String PROP_PRENODES = "preNodes";
    
    public static final String PROP_INTERRUPT = "interrupt";
    
    public static final String PROP_INTERRUPT_EXP = "interruptExp";
    
    
    public static final String PROP_RUNTIME_EVENT_NODE_ACTION_MAP = "_$eventNodeActionMap";
    public static final String PROP_RUNTIME_EVENT_POST_HANDLER_MAP = "_$eventPostHandlerMap";
    
    /**
     * Event param.
     */
    public static final String NODE_CONTEXT_DATA_EVENT = "_EVENT";

    public static final String NODE_PROP_EVENTS = "events";
    
    public static final String PARAM_DATA_EVENT = "_EVENT";
    
    public static final String NONE_EVENT = "NONE";
    
    /**
     * Node Pre checkType
     */
    public static final String NODE_PRE_CHECK_TYPE_MULTICHECK = "multiCheck";
    public static final String NODE_PRE_CHECK_TYPE_INCLUSIVECHECK = "inclusiveCheck";
    
    /**
     * initContextKey
     */
    public static final String INIT_CONTEXT_KEY = "initContext";
    
    
    public static final String FLOW_ENGINE_EVENT_DATA_KEY_PARAM = "param";
    public static final String FLOW_ENGINE_EVENT_DATA_KEY_FLOW_ENGINE = "flowEngine";
    public static final String FLOW_ENGINE_EVENT_DATA_KEY_RESULT = "result";
    public static final String FLOW_ENGINE_EVENT_DATA_KEY_EXCEPTION = "exception";


}
