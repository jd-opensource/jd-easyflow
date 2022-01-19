package com.jd.easyflow.flow.util;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowConstants {

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
    public static final String FLOW_CTX_MULTI_EXCEPTIONS = "flow.multi.exceptions";
    
    public static final String NODE_CTX_MULTI_EXCEPTION = "flow.multi.exception";
    
    
    public static final String CTX_LOCK_PREFIX = "_node_lock_";
    
    public static final String CTX_PRE_NODES_PREFIX = "_pre_nodes_";
    
    public static final String NODECTX_PRE_RESULT = "_preResult";
    
    public static final String PROP_PRENODES = "preNodes";
    
    public static final String PROP_RUNTIME_EVENT_NODE_ACTION_MAP = "$eventNodeActionMap";
    public static final String PROP_RUNTIME_EVENT_POST_HANDLER_MAP = "$eventPostHandlerMap";
    
    /**
     * Event param.
     */
    public static final String NODE_CONTEXT_DATA_EVENT = "EVENT";

    public static final String NODE_PROP_EVENTS = "events";
    
    public static final String PARAM_DATA_EVENT = "EVENT";
    
    public static final String NONE_EVENT = "NONE";


}
