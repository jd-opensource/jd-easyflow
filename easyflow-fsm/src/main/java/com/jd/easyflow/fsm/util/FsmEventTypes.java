package com.jd.easyflow.fsm.util;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmEventTypes {

	/**
	 * Common Event.
	 */
    public static final String FSM_MANAGER_START = "FSM_MANAGER_START";
    public static final String FSM_MANAGER_END = "FSM_MANAGER_END";
    public static final String FSM_MANAGER_COMPLETE = "FSM_MANAGER_COMPLETE";
    
	public static final String FSM_START = "FSM_START";
	public static final String FSM_END = "FSM_END";
	public static final String FSM_COMPLETE = "FSM_COMPLETE";

	public static final String TST_START = "TST_START";
	public static final String TST_END = "TST_END";
	public static final String TST_COMPLETE = "TST_COMPLETE";
	
	/**
	 * Extension Event.
	 */
    public static final String TST_PRE_START = "TST_PRE_START";
    public static final String TST_PRE_END = "TST_PRE_END";
    public static final String TST_ACTION_START = "TST_ACTION_START";
    public static final String TST_ACTION_END = "TST_ACTION_END";
    public static final String TST_POST_START = "TST_POST_START";
    public static final String TST_POST_END = "TST_POST_END";

}
