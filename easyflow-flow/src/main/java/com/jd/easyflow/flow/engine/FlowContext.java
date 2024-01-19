package com.jd.easyflow.flow.engine;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * Flow context.
 * @author liyuliang5
 *
 */
public interface FlowContext {
        
    /**
     * Get start node context list.
     * @return
     */
    List<NodeContext> getStartNodes();
    /**
     * Set start node context list.
     * @param startNodes
     */
    void setStartNodes(List<NodeContext> startNodes);
    /**
     * Get end node context list. 
     * From every start node，if execute to no next node(Including execute nodes)，we add it to end node list.
     * @return
     */
    List<NodeContext> getEndNodes();
    
    /**
     * Put context data.
     * @param key
     * @param value
     */
	void put(String key, Object value);
	/**
	 * Get context data.
	 * @param <T>
	 * @param key
	 * @return
	 */
	<T> T get(String key);
	
	/**
	 * Delete key.
	 * @param key
	 */
	void remove(String key);
	
	/**
	 * Set data.
	 * @param data
	 */
	void setData(Map<String, Object> data);
	
	/**
	 * Get data.
	 * @return
	 */
	Map<String, Object> getData();
	
	/**
	 * Get flow param.
	 * @return
	 */
	FlowParam getParam();
	
	/**
	 * Set flow param.
	 * @param param
	 */
	void setParam(FlowParam param);
	
	/**
	 * Get flow result.
	 * @return
	 */
	FlowResult getResult();

	/**
	 * Set flow result.
	 * @param result
	 */
	void setResult(FlowResult result);
	
	/**
	 * Get flow id.
	 * @return
	 */
	String getFlowId();
	
	/**
	 * Set flow id.
	 * @param flowId
	 */
	void setFlowId(String flowId);
	
	/**
	 * Get flow.
	 * @return
	 */
	Flow getFlow();
	
	/**
	 * Set flow.
	 * @param flow
	 */
	void setFlow(Flow flow);
	
	/**
	 * Get flow engine.
	 * @return
	 */
	FlowEngine getFlowEngine();
	
	/**
	 * Set interrupted. only can from false to true.
	 * @param interrupted
	 */
	void setInterrupted();
	
	/**
	 * Get interrupted.
	 * @return
	 */
	boolean isInterrupted();
	
	/**
	 * Get pre result.
	 * @return
	 */
	Boolean getPreResult();
	
	/**
	 * Get log flag.
	 * @return
	 */
	Boolean getLogFlag();

	/**
	 * Set log flag.
	 * @param logFlag
	 */
	void setLogFlag(Boolean logFlag);
	   
	/**
	 * 
	 * @return
	 */
	boolean isLogOn();
	
	/**
	 * Get business context.
	 * @param <T>
	 * @return
	 */
    <T>T getContext();

    /**
     * Set business context.
     * @param context
     */
    void setContext(Object context);


}
