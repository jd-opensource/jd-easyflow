package com.jd.easyflow.flow.model;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * Flow node init context.
 * @author liyuliang5
 *
 */
public class InitContext {

    private boolean parseEl;
    
    private FlowParser flowParser;
    
    private List<Flow> flowList;
    
    private Map<String, Object> flowDefinitionMap;
    
    private Flow flow;

    public boolean isParseEl() {
        return parseEl;
    }

    public void setParseEl(boolean parseEl) {
        this.parseEl = parseEl;
    }

    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }

    public List<Flow> getFlowList() {
        return flowList;
    }

    public void setFlowList(List<Flow> flowList) {
        this.flowList = flowList;
    }

    public Map<String, Object> getFlowDefinitionMap() {
        return flowDefinitionMap;
    }

    public void setFlowDefinitionMap(Map<String, Object> flowDefinitionMap) {
        this.flowDefinitionMap = flowDefinitionMap;
    }

    public Flow getFlow() {
        return flow;
    }

    public void setFlow(Flow flow) {
        this.flow = flow;
    }
    

}
