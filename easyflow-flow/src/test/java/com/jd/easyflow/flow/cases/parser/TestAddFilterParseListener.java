package com.jd.easyflow.flow.cases.parser;

import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;

/**
 * 
 * @author liyuliang5
 */
public class TestAddFilterParseListener implements FlowParseEventListener {

    @Override
    public void on(FlowParseEvent event) {
        switch (event.getType()) {
        case FlowParseEventTypes.INIT_FLOW_END: {
            event.getFlow().getFilterManager().addFilter(new TestFlowParamAndResultPrintFilter());
            break;
        }

        }

    }

}
