package com.jd.easyflow.flow.model.definition;

/**
 * 
 * @author liyuliang5
 *
 */
public class DefConstants {

    public static final String COMMON_PROP_ID = "id";
    public static final String COMMON_PROP_NAME = "name";
    public static final String COMMON_PROP_EXP = "exp";
    public static final String COMMON_PROP_FLOW = "flow";
    public static final String COMMON_PROP_FLOW_ID = "flowId";
    public static final String COMMON_PROP_CREATE_EXP = "createExp";
    public static final String COMMON_PROP_PROPERTIES = "properties";
    public static final String COMMON_PROP_CREATE = "create";
    public static final String COMMON_PROP_TYPE = "type";
    
    public static final String FLOW_PROP_PARSE_LISTENERS = "parseListeners";

    
    public static final String FLOW_PROP_PRE = "pre";
    public static final String FLOW_PROP_POST = "post";
    public static final String FLOW_PROP_NODES = "nodes";
    public static final String FLOW_PROP_LISTENERS = "listeners";
    public static final String FLOW_PROP_FILTERS = "filters";
    public static final String FLOW_PROP_NODE_FILTERS = "nodeFilters";
    public static final String FLOW_PROP_NODE_PRE_HANDLER_FILTERS = "nodePreHandlerFilters";
    public static final String FLOW_PROP_NODE_ACTION_FILTERS = "nodeActionFilters";
    public static final String FLOW_PROP_NODE_POST_HANDLER_FILTERS = "nodePostHandlerFilters";
    public static final String FLOW_PROP_FLOW_PRE_HANDLER_FILTERS = "flowPreHandlerFilters";
    public static final String FLOW_PROP_FLOW_POST_HANDLER_FILTERS = "flowPostHandlerFilters";

    public static final String FLOW_PROP_RUNNER = "runner";
    public static final String FLOW_PROP_LOG_FLAG = "logFlag";
    
    // Node property
    
    public static final String NODE_PROP_START = "start";
    
    public static final String NODE_PROPERTIES_PROP_END = "end";

    public static final String NODE_PROP_ACTION = "action";
    public static final String NODE_PROP_PRE = "pre";
    public static final String NODE_PROP_POST = "post";
    
    public static final String NODE_PROPERTIES_PROP_PRE_NODES = "preNodes";
    
    /**
     * Node pre property
     */
    public static final String NODE_PRE_TYPE_MULTICHECK = "multiCheck";
    public static final String NODE_PRE_TYPE_INCLUSIVECHECK = "inclusiveCheck";
    public static final String NODE_PRE_PROP_PRE_NODES = "preNodes";

    
    /**
     * Node action property
     */
    public static final String NODE_ACTION_TYPE_EVENT = "event";
    public static final String NODE_ACTION_TYPE_FLOW = "flow";
    public static final String NODE_ACTION_TYPE_INTERRUPT = "interrupt";
    public static final String NODE_ACTION_TYPE_LOOP = "loop";
    public static final String NODE_ACTION_TYPE_PARAM_EXECUTOR = "paramExecutor";
    public static final String NODE_ACTION_PROP_START_NODE_ID = "startNodeId";
    public static final String NODE_ACTION_PROP_INHERIT = "inherit";

    
    /**
     * Node post property
     */
    public static final String NODE_POST_TYPE_CONDITION = "condition";
    public static final String NODE_POST_TYPE_EVENT = "event";
    public static final String NODE_POST_PROP_CONDITIONS = "conditions";
    public static final String NODE_POST_PROP_WHEN = "when";
    public static final String NODE_POST_PROP_TO = "to";
    public static final String NODE_POST_TYPE_FIXED = "fixed";
    public static final String NODE_POST_PROP_CONDITION_TYPE = "conditionType";
    public static final String NODE_POST_PROP_DEFAULT_TO = "defaultTo";
}
