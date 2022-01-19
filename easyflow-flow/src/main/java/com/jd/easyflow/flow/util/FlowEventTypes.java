package com.jd.easyflow.flow.util;

/**
 * Flow event types.
 * @author liyuliang5
 *
 */
public class FlowEventTypes {

	/**
	 * Common events.
	 */
    public static final String FLOW_ENGINE_START = "FLOW_ENGINE_START";
    public static final String FLOW_ENGINE_END = "FLOW_ENGINE_END";
    public static final String FLOW_ENGINE_COMPLETE = "FLOW_ENGINE_COMPLETE";
    
	public static final String FLOW_START = "FLOW_START";
	public static final String FLOW_END = "FLOW_END";
	public static final String FLOW_COMPLETE = "FLOW_COMPLETE";
	
	public static final String INIT_START = "INIT_START";
	public static final String INIT_END = "INIT_END";
	
	public static final String RUN_START = "RUN_START";
	public static final String RUN_END = "RUN_END";
	
	public static final String NODE_START = "NODE_START";
	public static final String NODE_END = "NODE_END";
	public static final String NODE_COMPLETE = "NODE_COMPLETE";
	
	/**
	 * Extension events.
	 */
	public static final String NODE_PRE_START = "NODE_PRE_START";
	public static final String NODE_PRE_END = "NODE_PRE_END";
	public static final String NODE_ACTION_START = "NODE_ACTION_START";
	public static final String NODE_ACTION_END = "NODE_ACTION_END";
	public static final String NODE_POST_START = "NODE_POST_START";
	public static final String NODE_POST_END = "NODE_POST_END";
}
