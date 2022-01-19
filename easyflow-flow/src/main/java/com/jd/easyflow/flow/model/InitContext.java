package com.jd.easyflow.flow.model;

import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * Flow node init context.
 * @author liyuliang5
 *
 */
public class InitContext {

    private boolean parseEl;
    
    FlowParser flowParser;

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
    
    
}
