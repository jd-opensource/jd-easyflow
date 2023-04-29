package com.jd.easyflow.flow.model.parser;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.model.parser.param.ActionParseParam;
import com.jd.easyflow.flow.model.parser.param.FlowParseParam;
import com.jd.easyflow.flow.model.parser.param.PostParseParam;
import com.jd.easyflow.flow.model.parser.param.PreParseParam;

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
     * Parse definition map to java model.
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
     * Parse definition map to java model.
     * @param map
     * @param parseEl
     * @return
     */
    List<Flow> parse(Map<String, Object> map, boolean parseEl);
    
    /**
     * General parse method.
     * @param param
     * @return
     */
    List<Flow> parse(FlowParseParam param);
    
    
    /**
     * Parse preHandler.
     * @param param
     * @return
     */
    NodePreHandler parseNodePreHandler(PreParseParam param);
    
    /**
     * Parse nodeAction.
     * @param param
     * @return
     */
    NodeAction parseNodeAction(ActionParseParam param);

    /**
     * Parse postHandler.
     * @param param.
     * @return
     */
    NodePostHandler parseNodePostHandler(PostParseParam param);

    /**
     * 
     * Convert java model to string.
     *
     * @param flow
     * @return
     */
    String stringify(Flow flow);

}
