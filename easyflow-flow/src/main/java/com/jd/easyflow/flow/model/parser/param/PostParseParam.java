package com.jd.easyflow.flow.model.parser.param;

import com.jd.easyflow.flow.model.FlowNode;

/**
 * 
 * @author liyuliang5
 *
 */
public class PostParseParam {

    private Object postDef;

    private boolean parseEl;
    
    private FlowNode node;

    public PostParseParam() {
        // NOOP
    }

    public PostParseParam(Object postDef, boolean parseEl, FlowNode node) {
        this.postDef = postDef;
        this.parseEl = parseEl;
        this.node = node;
    }

    public Object getPostDef() {
        return postDef;
    }

    public void setPostDef(Object postDef) {
        this.postDef = postDef;
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
