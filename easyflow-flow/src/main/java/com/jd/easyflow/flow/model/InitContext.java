package com.jd.easyflow.flow.model;

import java.util.List;

import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * Flow node init context.
 * @author liyuliang5
 *
 */
public class InitContext {

    private boolean parseEl;
    
    FlowParser flowParser;
    
    List<Flow> flowList;

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
    
    
}
