package com.jd.easyflow.flow.model.parser;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;

/**
 * Flow definition parser.
 * 
 * @author liyuliang5
 * @version 1.0
 * @since 1.0
 */
public interface FlowParser {

    /**
     * Parse definition string to java model.
     * 
     * @param data
     * @return
     */
    List<Flow> parse(String data);
    
    /**
     * Parse definition string to java model.
     * @param map
     * @return
     */
    List<Flow> parse(Map<String, Object> map);
    
    /**
     * Parse definition string to java model.
     * @param data
     * @param parseEl
     * @return
     */
    List<Flow> parse(String data, boolean parseEl);
    
    /**
     * Parse definition string to java model.
     * @param map
     * @param parseEl
     * @return
     */
    List<Flow> parse(Map<String, Object> map, boolean parseEl);
    
    /**
     * Parse preHandler.
     * @param preParam
     * @param parseEl
     * @return
     */
    NodePreHandler parsePre(Object preParam, boolean parseEl);
    
    /**
     * Parse nodeAction.
     * @param actionParam
     * @param flowList
     * @param parseEl
     * @return
     */
    NodeAction parseAction(Object actionParam, List<Flow> flowList, boolean parseEl);

    /**
     * Parse postHandler.
     * @param postParam
     * @param parseEl
     * @return
     */
    NodePostHandler parsePost(Object postParam, boolean parseEl);

    /**
     * 
     * Convert java model to string.
     *
     * @param flow
     * @return
     */
    String stringify(Flow flow);

}
