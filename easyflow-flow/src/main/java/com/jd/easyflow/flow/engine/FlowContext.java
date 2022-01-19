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
     * Set end node list(ONLY FOR INNER USAGE).
     * @param endNodes
     */
    void setEndNodes(List<NodeContext> endNodes);
    
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
	 * Add nodes.
	 * @param nodes
	 */
	void addNodes(NodeContext[] nodes);
	/**
	 * Add end node context list.
	 * @param node
	 */
	void addEndNode(NodeContext node);
	
	/**
	 * Get next node context.
	 * @return
	 */
	NodeContext getNextNode();
	
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
	 * Set flow engine.
	 * @param flowEngine
	 */
	void setFlowEngine(FlowEngine flowEngine);

}
