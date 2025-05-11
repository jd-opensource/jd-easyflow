package com.jd.easyflow.flow.model.parser.event;

import java.util.Map;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowParseEvent {
    
    private String type;
    
    private Map<String, Object> flowDef;
    
    private Flow flow;
    
    private Object data;
    
    private FlowParser flowParser;
    
    private boolean parseEl;
    
    
    public Flow getFlow() {
        return flow;
    }

    public void setFlow(Flow flow) {
        this.flow = flow;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Map<String, Object> getFlowDef() {
        return flowDef;
    }

    public void setFlowDef(Map<String, Object> flowDef) {
        this.flowDef = flowDef;
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }

    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }

    public boolean isParseEl() {
        return parseEl;
    }

    public void setParseEl(boolean parseEl) {
        this.parseEl = parseEl;
    }
    
    

}
