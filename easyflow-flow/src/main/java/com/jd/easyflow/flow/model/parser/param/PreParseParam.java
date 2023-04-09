package com.jd.easyflow.flow.model.parser.param;

import com.jd.easyflow.flow.model.FlowNode;

/**
 * 
 * @author liyuliang5
 *
 */
public class PreParseParam {

    private Object preDef;

    private boolean parseEl;
    
    private FlowNode node;
    
    public PreParseParam() {
        // NOOP
    }
    
    public PreParseParam(Object preDef, boolean parseEl, FlowNode node) {
        this.preDef = preDef;
        this.parseEl = parseEl;
        this.node = node;
    }
    

    public Object getPreDef() {
        return preDef;
    }

    public void setPreDef(Object preDef) {
        this.preDef = preDef;
    }

    public boolean isParseEl() {
        return parseEl;
    }

    public void setParseEl(boolean parseEl) {
        this.parseEl = parseEl;
    }

    public FlowNode getNode() {
        return node;
    }

    public void setNode(FlowNode node) {
        this.node = node;
    }
    

}
