package com.jd.easyflow.flow.model.parser.param;

import java.util.List;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;

/**
 * 
 * @author liyuliang5
 *
 */
public class PostParseParam {

    private Object postDef;
    
    private List<Flow> flowList;

    private boolean parseEl;
    
    private Flow flow;
    
    private FlowNode node;

    public PostParseParam() {
        // NOOP
    }

    public PostParseParam(Object postDef,List<Flow> flowList, boolean parseEl, Flow flow, FlowNode node) {
        this.postDef = postDef;
        this.flowList = flowList;
        this.parseEl = parseEl;
        this.flow = flow;
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

    public List<Flow> getFlowList() {
        return flowList;
    }

    public void setFlowList(List<Flow> flowList) {
        this.flowList = flowList;
    }

    public Flow getFlow() {
        return flow;
    }

    public void setFlow(Flow flow) {
        this.flow = flow;
    }
    
}
