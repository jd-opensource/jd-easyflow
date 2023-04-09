package com.jd.easyflow.flow.model.parser.param;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowParseParam {

    private String stringDefinition;

    private Object objectDefinition;

    private boolean parseEl = true;

    public FlowParseParam() {
        // NOOP
    }

    public FlowParseParam(String stringDefinition) {
        this.stringDefinition = stringDefinition;
    }

    public FlowParseParam(Object objectDefinition) {
        this.objectDefinition = objectDefinition;
    }

    public FlowParseParam(String stringDefinition, boolean parseEl) {
        this.stringDefinition = stringDefinition;
        this.parseEl = parseEl;
    }

    public FlowParseParam(Object objectDefinition, boolean parseEl) {
        this.objectDefinition = objectDefinition;
        this.parseEl = parseEl;
    }

    public String getStringDefinition() {
        return stringDefinition;
    }

    public void setStringDefinition(String stringDefinition) {
        this.stringDefinition = stringDefinition;
    }


    public Object getObjectDefinition() {
        return objectDefinition;
    }

    public void setObjectDefinition(Object objectDefinition) {
        this.objectDefinition = objectDefinition;
    }

    public boolean isParseEl() {
        return parseEl;
    }

    public void setParseEl(boolean parseEl) {
        this.parseEl = parseEl;
    }

}
