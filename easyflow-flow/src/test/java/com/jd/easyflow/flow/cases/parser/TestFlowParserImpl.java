package com.jd.easyflow.flow.cases.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;

/**
 * 
 * @author liyuliang5
 */
public class TestFlowParserImpl extends FlowParserImpl {

    @Override
    protected List<FlowParseEventListener> parseParseListeners(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<FlowParseEventListener> list = super.parseParseListeners(map, flow, parseEl);
        if (list == null) {
            list = new ArrayList<FlowParseEventListener>();
        }
        list.add(new TestAddFilterParseListener());
        return list;
    }
}
