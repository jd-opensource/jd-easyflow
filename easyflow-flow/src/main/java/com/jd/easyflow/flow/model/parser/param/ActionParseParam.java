package com.jd.easyflow.flow.model.parser.param;

import java.util.List;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * 
 * @author liyuliang5
 *
 */
public class ActionParseParam {

    private Object actionDef;

    private List<Flow> flowList;

    private boolean parseEl;
    
    private Flow flow;
    
    private FlowNode node;

    public ActionParseParam() {
        // NOOP
    }

    public ActionParseParam(Object actionDef, List<Flow> flowList, boolean parseEl, Flow flow, FlowNode node) {
        this.actionDef = actionDef;
        this.flowList = flowList;
        this.parseEl = parseEl;
        this.flow = flow;
        this.node = node;
    }

    public Object getActionDef() {
        return actionDef;
    }

    public void setActionDef(Object actionDef) {
        this.actionDef = actionDef;
    }

    public List<Flow> getFlowList() {
        return flowList;
    }

    public void setFlowList(List<Flow> flowList) {
        this.flowList = flowList;
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

    public Flow getFlow() {
        return flow;
    }

    public void setFlow(Flow flow) {
        this.flow = flow;
    }
    
}
